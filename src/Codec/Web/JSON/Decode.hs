{- | Low-level JSON deserialization.

     Functions in this module assume that raw JSON is UTF-8 encoded,
     as per section 8.1 of RFC 8259.

     'Decoder's this module exports are conservative in the types of JSON values
     they decode from. For more open formats see "Codec.Web.JSON.Decode.Questionable".

     String 'Decoder's in this module all fail immediately upon encountering an invalid
     character sequence. For 'Decoder's with more relaxed string decoding handling see
     "Codec.Web.JSON.Decode.Encoding".

     For direct access to underlying data structures see "Codec.Web.JSON.Decode.Core".
 -}

module Codec.Web.JSON.Decode
  ( -- * Decoder
    Decoder

  , Path (..)
  , Result (..)
  , decode
  , decodeM

    -- ** Streaming
  , Stream
  , stream

  , Source (..)
  , source
  , sourceF

    -- * Value
  , rawValue
  , skipValue

    -- ** Object
  , rawObject
  , skipObject

    -- *** Pair
  , Name
  , stringName
  , textName
  , lazyTextName

  , Pair ((.:))
  , (.:?)

    -- *** Plain
  , Pairs
  , object

    -- *** Bounded
  , BoundedPairs
  , boundedFail
  , boundedObject

    -- *** Linear
  , LinearPairs
  , linearObject

    -- *** Fold
  , FoldName
  , stringFoldName
  , textFoldName
  , lazyTextFoldName
  , rawFoldName

  , foldObject
  , streamObject

    -- ** Array
  , rawArray
  , skipArray

    -- *** Plain
  , array

    -- *** Elements
  , Elements
  , element
  , mayElement
  , elementArray

    -- *** Fold
  , foldArray
  , streamArray

    -- ** String
  , rawString
  , skipString

    -- *** Plain
  , Codec.Web.JSON.Decode.string
  , byteString
  , lazyByteString
  , text
  , lazyText

    -- *** Time
  , year

  , quarter

  , month

  , week

  , calendarDate
  , weekDate
  , ordinalDate

  , timeOfDay
  , timeZone

  , utcTime
  , localTime
  , zonedTime

    -- *** UUID
  , uuid

    -- ** Number
  , rawNumber
  , skipNumber

    -- *** Integer
    -- **** Unsigned
  , Codec.Web.JSON.Decode.word8
  , word16
  , word32
  , word64
  , word
  , natural
    -- **** Signed
  , int8
  , int16
  , int32
  , int64
  , int
  , integer

    -- *** Floating-point
  , float
  , double
  , scientific

    -- ** Boolean
  , boolean

    -- ** Null
  , Codec.Web.JSON.Decode.null
  , nullOr
  ) where

import           Codec.Web.JSON.Decode.Array.Core
import           Codec.Web.JSON.Decode.Core
import           Codec.Web.JSON.Decode.Encoding
import           Codec.Web.JSON.Decode.Array.Elements
import           Codec.Web.JSON.Decode.Object.Bounded
import           Codec.Web.JSON.Decode.Object.Core
import           Codec.Web.JSON.Decode.Object.Linear
import           Codec.Web.JSON.Decode.Object.Plain
import           Codec.Web.JSON.Parse.Boolean
import           Codec.Web.JSON.Parse.Knot
import           Codec.Web.JSON.Parse.Number
import           Codec.Web.JSON.Parse.Null
import           Codec.Web.JSON.Parse.Time
import           Codec.Web.JSON.Parse.UUID
import           Data.ByteString.Lazy.Copy

import qualified Data.Attoparsec.ByteString as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Internal as BSL (ByteString (..), chunk)
import           Data.Int
import           Data.Scientific (Scientific)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import           Data.Time.Calendar (Day)
import           Data.Time.Calendar.Compat (Year)
import           Data.Time.Calendar.Month.Compat (Month)
import           Data.Time.Calendar.Quarter.Compat (Quarter)
import           Data.Time.Calendar.WeekDate.Compat (WeekOfYear)
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime (TimeOfDay, TimeZone, LocalTime, ZonedTime)
import           Data.UUID.Types
import           Data.Word
import           Numeric.Natural



defaultChunkSize :: Int
defaultChunkSize = 1024



-- | Simple decoding result. @r@ is the remaining pure part of the input.
data Result r a = Success r a
                | Failure r Path String
                  deriving Show

instance Functor (Result r) where
  fmap f (Success r a)        = Success r (f a)
  fmap _ (Failure r path msg) = Failure r path msg



-- | Run a 'Decoder' over a 'BSL.LazyByteString'.
--
--   Input is consumed lazily.
decode :: Decoder a -> BSL.ByteString -> Result BSL.ByteString a
decode (Decoder decoder) bs0 =
  case bs0 of
    BSL.Chunk c cs -> go cs $ Atto.parse decoder c
    BSL.Empty      -> go BSL.empty $ Atto.parse decoder BS.empty
  where
    go bs result =
      case result of
        Atto.Done r a   -> Success (BSL.chunk r bs) a
        Atto.Fail r c e -> Failure (BSL.chunk r bs) (Path $ '$' : mconcat c) e
        Atto.Partial f  -> case bs of
                             BSL.Chunk c cs -> go cs (f c)
                             BSL.Empty      -> go BSL.empty (f BS.empty)



-- | Run a 'Decoder' over a 'BSL.LazyByteString' chunked 'Monad'ically.
--
--   Input is consumed lazily.
--   First empty 'BS.StrictByteString' is treated as end of input.
decodeM :: Monad m => Decoder a -> m BS.ByteString -> m (Result BS.ByteString a)
decodeM (Decoder decoder) get = do
  bs <- get
  go $ Atto.parse decoder bs
  where
    go result =
      case result of
        Atto.Done r a   -> pure $ Success r a
        Atto.Fail r c e -> pure $ Failure r (Path $ '$' : mconcat c) e
        Atto.Partial f  -> do bs <- get
                              go $ f bs



-- | Embed a 'Decoder' as a single action into a 'Stream'.
stream :: Decoder (a, b) -> Stream a b
stream (Decoder decoder) = Parse $ (\ ~(a, b) -> Yield a $ Return b) <$> decoder



-- | Iterable consumption of a 'Stream'. @r@ is the remaining pure part of the input.
data Source f r a b = Step a (Source f r a b)
                    | Effect (f (Source f r a b))
                    | Done r b
                    | Failed r Path String



-- | Run a 'Stream' over 'BSL.LazyByteString'.
--
--   Input is consumed lazily. Parsing advances on demand as 'Source' is unwrapped.
source :: Stream a b -> BSL.ByteString -> Source f BSL.ByteString a b
source s0 bs0 = unwrap bs0 s0
  where
    unwrap bs s =
      case s of
        Yield a s' -> Step a $ unwrap bs s'
        Parse e    -> go bs (Atto.Partial $ Atto.parse e)
        Return r   -> Done bs r

    go bs r =
      case r of
        Atto.Done b s'  -> unwrap (BSL.chunk b bs) s'
        Atto.Fail b c e -> Failed (BSL.chunk b bs) (Path $ '$' : mconcat c) e
        Atto.Partial f  -> case bs of
                             BSL.Chunk c cs -> go cs $ f c
                             BSL.Empty      -> go BSL.Empty $ f BS.empty



-- | Run a 'Stream' over 'BSL.LazyByteString' chunked 'Monad'ically.
--
--   Input is consumed lazily.
--   First empty 'BS.StrictByteString' is treated as end of input.
--   Parsing advances on demand as 'Source' is unwrapped.
sourceF :: Functor f => Stream a b -> f BS.ByteString -> Source f BS.ByteString a b
sourceF s0 get = unwrap BS.empty s0
  where
    unwrap bs s =
      case s of
        Yield a s' -> Step a $ unwrap bs s'
        Parse e    -> if BS.null bs
                        then Effect $ go . Atto.parse e <$> get
                        else go $ Atto.parse e bs

        Return r   -> Done bs r

    go r =
      case r of
        Atto.Done b s'  -> unwrap b s'
        Atto.Fail b c e -> Failed b (Path $ '$' : mconcat c) e
        Atto.Partial f  -> Effect $ go . f <$> get



-- | Parses a JSON value and returns it verbatim.
rawValue :: Decoder BSL.ByteString
rawValue = rawValue' strict defaultChunkSize

-- | Parses a JSON value and discards it.
skipValue :: Decoder ()
skipValue = skipValue' strict



-- | Create a 'Name' from a 'String'.
stringName :: String -> Name
stringName = Name

-- | Create a 'Name' from a strict 'Text'.
textName :: Text -> Name
textName = Name . Text.unpack

-- | Create a 'Name' from a lazy 'Lazy.Text'.
lazyTextName :: Lazy.Text -> Name
lazyTextName = Name . Lazy.unpack



-- | Decodes a value under 'Name' in an object.
--
--   If no such 'Name' is present in the object or the value is @null@,
--   returns a 'Nothing'.
(.:?) :: Pair f => Name -> Decoder a -> f (Maybe a)
(.:?) name = (.:??) name . nullOr



-- | Parses a JSON object.
--
--   Duplicate 'Name's do not cause a failure, instead they are skipped.
--
--   This combinator copies pair values for 'Name's mentioned in 'BoundedPairs' and
--   only starts resolving said pairs after it reaches the end of the object.
boundedObject :: BoundedPairs a -> Decoder a
boundedObject = boundedObject' strict defaultChunkSize


-- | Parses a JSON object.
--
--   Duplicate 'Name's do not cause a failure, instead they are skipped.
--
--   If any 'Name' within 'LinearPairs' is mentioned twice, fails immediately.
--
--   'Decoder's are ran as their respective 'Name's are encountered.
--   This combinator does not do any underlying input copying and thus runs
--   in constant space.
linearObject :: LinearPairs a -> Decoder a
linearObject = linearObject' strict


-- | Parses a JSON object.
--
--   Duplicate 'Name's do not cause a failure, instead they are skipped.
--
--   This combinator copies every single pair value it comes across and only
--   starts resolving the 'Pairs' after it reaches the end of the object.
object :: Pairs a -> Decoder a
object = object' strict defaultChunkSize



-- | Parses a JSON object and returns it verbatim.
rawObject :: Decoder BSL.ByteString
rawObject = rawObject' strict defaultChunkSize

-- | Parses a JSON object and discards it.
skipObject :: Decoder ()
skipObject = skipObject' strict



-- | Decode object pair name as a 'String'.
stringFoldName :: FoldName String
stringFoldName = FoldName string id

-- | Decode object pair name as a strict 'Text'.
textFoldName :: FoldName Text
textFoldName = FoldName text Text.unpack

-- | Decode object pair name as a lazy 'Lazy.Text'.
lazyTextFoldName :: FoldName Lazy.Text
lazyTextFoldName = FoldName lazyText Lazy.unpack

-- | Decode object pair name as a raw JSON string returned verbatim.
--
--   Double quotes around the string are preserved.
rawFoldName :: FoldName BSL.ByteString
rawFoldName = FoldName rawString $ \raw ->
                case decode (string' lenient) raw of
                  Success _ res -> res
                  Failure _ _ _ -> "???" -- As long as 'rawString' functions correctly
                                         -- the failure is unreachable.



-- | Parses a JSON array and returns it verbatim.
rawArray :: Decoder BSL.ByteString
rawArray = rawArray' strict defaultChunkSize

-- | Parses a JSON array and discards it.
skipArray :: Decoder ()
skipArray = skipArray' strict



-- | Parses a JSON string as a 'String'.
string :: Decoder String
string = string' strict

-- | Parses a JSON string as a UTF-8 encoded 'BS.StrictByteString'.
byteString :: Decoder ByteString
byteString = byteString' strict

-- | Parses a JSON string as a UTF-8 encoded 'BSL.LazyByteString'.
lazyByteString :: Decoder BSL.ByteString
lazyByteString = lazyByteString' strict defaultChunkSize

-- | Parses a JSON string as a strict 'Text'.
text :: Decoder Text
text = text' strict

-- | Parses a JSON string as a lazy 'Lazy.Text'.
lazyText :: Decoder Lazy.Text
lazyText = lazyText' strict defaultChunkSize



-- | Parses a JSON string and returns it verbatim.
--
--   Double quotes around the string are preserved.
rawString :: Decoder BSL.ByteString
rawString = rawString' strict defaultChunkSize



-- | Parses a JSON string and discards it.
skipString :: Decoder ()
skipString = skipString' strict



-- | Parses a JSON string as a 'Year'.
--
--   Expected format is @yyyy@.
year :: Decoder Year
year = withString yearP

-- | Parses a JSON string as a 'Quarter'.
--
--   Expected format is @yyyy-Qq@ with @q@ in @1-4@ range.
quarter :: Decoder Quarter
quarter = withString quarterP

-- | Parses a JSON string as a 'Month'.
--
--   Expected format is @yyyy-mm@ with @mm@ in @01-12@ range.
month :: Decoder Month
month = withString monthP

-- | Parses a JSON string as a week.
--
--   Expected format is @yyyy-Www@ with @ww@ in @01-53@ range.
--   Fails if week @53@ is used for a year that has no leap week.
week :: Decoder (Year, WeekOfYear)
week = withString weekP

-- | Parses a JSON string as a calendar date.
--
--   Expected format is @yyyy-mm-dd@ with @yyyy-mm@ as 'month' and @dd@ in @01-31@ range.
--   Fails if a day is used for a month that does not have it.
calendarDate :: Decoder Day
calendarDate = withString calendarDateP

-- | Parses a JSON string as a week date.
--
--   Expected format is @yyyy-Www-d@ with @yyyy-Www@ as 'week' and @d@ in @1-7@ range.
weekDate :: Decoder Day
weekDate = withString weekDateP

-- | Parses a JSON string as an ordinal date.
--
--   Expected format is @yyyy-ddd@ with @ddd@ in @01-366@ range.
--   Fails if day @366@ is used for a year that has no leap day.
ordinalDate :: Decoder Day
ordinalDate = withString ordinalDateP

-- | Parses a JSON string as a 'TimeOfDay'.
--
--   Expected format is @hh-mm-ss[.sss]@ with
--   @hh@ in @00-23@ range, @mm@ in @00-59@ range and @ss@ in @00-60@ range.
--
--   Only stores seconds up to picosecond precision, further digits are skipped.
timeOfDay :: Decoder TimeOfDay
timeOfDay = withString timeOfDayP

-- | Parses a JSON string as a 'TimeZone'.
--
--   Expected format is @±hh:mm@.
timeZone :: Decoder TimeZone
timeZone = withString timeZoneP

-- | Parses a JSON string as a 'UTCTime'.
--
--   Expected format is @yyyy-mm-ddThh-mm-ss[.sss]Z@ with @yyyy-mm-dd@ as 'calendarDate'
--   and @hh-mm-ss[.sss]@ as 'timeOfDay'.
--   @T@ may be @t@ or space, @Z@ may be @z@.
utcTime :: Decoder UTCTime
utcTime = withString utcTimeP

-- | Parses a JSON string as a 'LocalTime'.
--
--   Expected format is @yyyy-mm-ddThh-mm-ss[.sss]@ with @yyyy-mm-dd@ as 'calendarDate'
--   and @hh-mm-ss[.sss]@ as 'timeOfDay'.
--   @T@ may be @t@ or space.
localTime :: Decoder LocalTime
localTime = withString localTimeP

-- | Parses a JSON string as a 'ZonedTime'.
--
--   Expected format is @yyyy-mm-ddThh-mm-ss[.sss]±hh:mm@ with
--   @yyyy-mm-dd@ as 'calendarDate', @hh-mm-ss[.sss]@ as 'timeOfDay' and
--   @±hh:mm@ as 'timeZone'.
--   @T@ may be @t@ or space.
zonedTime :: Decoder ZonedTime
zonedTime = withString zonedTimeP



-- | Parses a JSON string as a 'UUID'.
--
--   Expected format is @uuuuuuuu-uuuu-uuuu-uuuu-uuuuuuuuuuuu@.
uuid :: Decoder UUID
uuid = withString uuidP



-- | Parses a JSON number as a 'Word8'.
--
--   Negative zero is a valid input.
word8 :: Decoder Word8
word8 = withNumber word8P

-- | Parses a JSON number as a 'Word16'.
--
--   Negative zero is a valid input.
word16 :: Decoder Word16
word16 = withNumber word16P

-- | Parses a JSON number as a 'Word32'.
--
--   Negative zero is a valid input.
word32 :: Decoder Word32
word32 = withNumber word32P

-- | Parses a JSON number as a 'Word64'.
--
--   Negative zero is a valid input.
word64 :: Decoder Word64
word64 = withNumber word64P

-- | Parses a JSON number as a 'Word'.
--
--   Negative zero is a valid input.
word :: Decoder Word
word = withNumber wordP

-- | Parses a JSON number as a 'Natural'.
--
--   Negative zero is a valid input.
natural :: Decoder Natural
natural = withNumber naturalP



-- | Parses a JSON number as an 'Int8'.
int8 :: Decoder Int8
int8 = withNumber int8P

-- | Parses a JSON number as an 'Int16'.
int16 :: Decoder Int16
int16 = withNumber int16P

-- | Parses a JSON number as an 'Int32'.
int32 :: Decoder Int32
int32 = withNumber int32P

-- | Parses a JSON number as an 'Int64'.
int64 :: Decoder Int64
int64 = withNumber int64P

-- | Parses a JSON number as an 'Int'.
int :: Decoder Int
int = withNumber intP

-- | Parses a JSON number as an 'Integer'.
integer :: Decoder Integer
integer = withNumber integerP



-- | Parses a JSON number as a 'Float'.
--
--   Returns infinity or zero if the number is too large or too small respectively.
--   Honors negative zero.
float :: Decoder Float
float = withNumber floatP

-- | Parses a JSON number as a 'Double'.
--
--   Returns infinity or zero if the number is too large or too small respectively.
--   Honors negative zero.
double :: Decoder Double
double = withNumber doubleP

-- | Parses a JSON number as a 'Scientific'.
--
--   Does not honor negative zero because 'Scientific' has no notion of such a state.
--
--   Fails if the exponent cannot fit into an 'Int'.
scientific :: Decoder Scientific
scientific = withNumber scientificP



-- | Parses a JSON number and returns it verbatim.
rawNumber :: Decoder BSL.ByteString
rawNumber =
  Decoder .
    copyingNumberP defaultChunkSize emptyCopy $ \copy0 -> do
      copy1 <- copyNumberP defaultChunkSize copy0
      pure $! commitCopy copy1



-- | Parses a JSON number and discards it.
skipNumber :: Decoder ()
skipNumber = withNumber skipNumberP



-- | Parses a JSON boolean as a 'Bool'.
boolean :: Decoder Bool
boolean = withBoolean $ \true ->
            true <$ if true
                      then trueP
                      else falseP



-- | Parses a JSON null.
null :: Decoder ()
null = withNull nullP

-- | Parses a JSON null as 'Nothing' or runs the 'Decoder'.
nullOr :: Decoder a -> Decoder (Maybe a)
nullOr (Decoder decoder) =
  withValue $ \v ->
    case v of
      X -> Nothing <$ nullP
      _ -> Just <$> decoder
