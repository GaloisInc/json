{- | Low-level JSON serialization.

     For direct access to underlying data structures see "Codec.Web.JSON.Encode.Core".
 -}

module Codec.Web.JSON.Encode
  ( -- * Encoder
    Encoder
  , Codec.Web.JSON.Encode.encode

    -- * Values
    -- ** Object
  , Name
  , stringName
  , textName
  , lazyTextName
  , stringUtf16Name
  , textUtf16Name
  , lazyTextUtf16Name

  , Pair (..)
  , EncoderPair
  , object

    -- ** Array
  , array

    -- ** String

    -- *** UTF-8
  , string
  , text
  , lazyText

    -- *** UTF-16
  , stringUtf16
  , textUtf16
  , lazyTextUtf16

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
    -- *** Integer
    -- **** Unsigned
  , Codec.Web.JSON.Encode.word8
  , word16
  , word32
  , word64
  , word
  , natural

    -- **** Signed
  , Codec.Web.JSON.Encode.int8
  , int16
  , int32
  , int64
  , int
  , integer

    -- *** Floating-point
  , float
  , double
  , Codec.Web.JSON.Encode.scientific

    -- ** Boolean
  , boolean

    -- ** Null
  , Codec.Web.JSON.Encode.null
  ) where

import           Codec.Web.JSON.Build.Boolean
import           Codec.Web.JSON.Build.Null
import           Codec.Web.JSON.Build.String
import           Codec.Web.JSON.Build.Time
import           Codec.Web.JSON.Build.UUID
import           Codec.Web.JSON.Encode.Core

import           Data.ByteString.Builder as Build
import qualified Data.ByteString.Builder.Prim as Prim
import           Data.ByteString.Builder.Scientific
import qualified Data.ByteString.Lazy as BSL
import           Data.Int
import           Data.Scientific
import           Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import           Data.Time.Calendar
import           Data.Time.Calendar.Quarter.Compat
import           Data.Time.Calendar.Month.Compat
import           Data.Time.Calendar.OrdinalDate.Compat
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Word
import           Data.UUID.Types (UUID)
import           Numeric.Natural



-- | Convert 'Encoder' into a 'BSL.LazyByteString'.
--
--   Output is produced lazily.
encode :: Encoder -> BSL.ByteString
encode = toLazyByteString . runEncoder



-- | Encodes 'String' as a JSON object pair name.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-8.
stringName :: String -> Name
stringName = Name . stringUtf8B

-- | Encodes strict 'Text' as a JSON object pair name.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-8.
textName :: Text -> Name
textName = Name . textUtf8B

-- | Encodes lazy 'Lazy.Text' as a JSON object pair name.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-8.
lazyTextName :: Lazy.Text -> Name
lazyTextName = Name . lazyTextUtf8B


-- | Encodes 'String' as a JSON object pair name.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-16 character escapes.
stringUtf16Name :: String -> Name
stringUtf16Name = Name . stringUtf16B

-- | Encodes strict 'Text' as a JSON object pair name.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-16 character escapes.
textUtf16Name :: Text -> Name
textUtf16Name = Name . textUtf16B

-- | Encodes lazy 'Lazy.Text' as a JSON object pair name.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-16 character escapes.
lazyTextUtf16Name :: Lazy.Text -> Name
lazyTextUtf16Name = Name . lazyTextUtf16B



-- | Encodes a list of 'EncoderPair's into a JSON object.
--
--   Pairs without an encoder are not included into the resulting object.
object :: [EncoderPair] -> Encoder
object pairs =
  Encoder $ Build.word8 0x7b {- { -} <> inter pairs <> Build.word8 0x7d {- } -}
  where
    {-# INLINE two #-}
    two = Prim.word8 Prim.>*< Prim.word8

    inter (EncoderPair (Name name) mayVal : xs) =
      case mayVal of
        Just val -> Build.word8 0x22 {- " -} <> name
                 <> Prim.primFixed two (0x22 {- " -}, 0x3a {- : -}) <> runEncoder val
                 <> inter1 xs

        Nothing  -> inter xs

    inter                     []  = mempty

    inter1 (EncoderPair (Name name) mayVal : xs) =
      case mayVal of
        Just val -> Prim.primFixed two (0x2c {- , -}, 0x22 {- " -}) <> name
                 <> Prim.primFixed two (0x22 {- " -}, 0x3a {- : -}) <> runEncoder val
                 <> inter1 xs

        Nothing   -> inter1 xs

    inter1                    []  = mempty



-- | Encodes a list of 'Encoder's into a JSON array.
array :: [Encoder] -> Encoder
array (v:vs) = Encoder $ Build.word8 0x5b {- [ -} <> inter v vs <> Build.word8 0x5d {- ] -}
  where
    inter x    []  = runEncoder x
    inter x (y:zs) = runEncoder x <> Build.word8 0x2c {- , -} <> inter y zs

array    []  = Encoder $ Prim.primMapListFixed Prim.word8 [0x5b {- [ -}, 0x5d {- ] -}]



-- | Encodes 'String' as a JSON string.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-8.
string :: String -> Encoder
string = encodeString . stringUtf8B

-- | Encodes strict 'Text' as a JSON string.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-8.
text :: Text -> Encoder
text = encodeString . textUtf8B

-- | Encodes lazy 'Lazy.Text' as a JSON string.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-8.
lazyText :: Lazy.Text -> Encoder
lazyText = encodeString . lazyTextUtf8B


-- | Encodes 'String' as a JSON string.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-16 character escapes.
stringUtf16 :: String -> Encoder
stringUtf16 = encodeString . stringUtf16B

-- | Encodes strict 'Text' as a JSON string.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-16 character escapes.
textUtf16 :: Text -> Encoder
textUtf16 = encodeString . textUtf16B

-- | Encodes lazy 'Lazy.Text' as a JSON string.
--
--   Characters in @U+7E@ to @U+10FFFF@ range are encoded as UTF-16 character escapes.
lazyTextUtf16 :: Lazy.Text -> Encoder
lazyTextUtf16 = encodeString . lazyTextUtf16B



-- | Encodes 'Year' as a JSON string.
--
--   Format is @yyyy@.
--
--   Always pads to four digits regardless of the sign, does not fail on over-/underflow.
year :: Year -> Encoder
year = encodeString . yearB

-- | Encodes 'Quarter' as a JSON string.
--
--   Format is @yyyy-Qq@.
quarter :: Quarter -> Encoder
quarter = encodeString . quarterB

-- | Encodes 'Month' as a JSON string.
--
--   Format is @yyyy-mm@.
month :: Month -> Encoder
month = encodeString . monthB

-- | Encodes week as a JSON string.
--
--   Format is @yyyy-Www@.
week :: Year -> WeekOfYear -> Encoder
week y = encodeString . weekB y

-- | Encodes calendar date as a JSON string.
--
--   Format is @yyyy-mm-dd@.
calendarDate :: Day -> Encoder
calendarDate = encodeString . calendarDateB

-- | Encodes week date as a JSON string.
--
--   Format is @yyyy-Www-d@.
weekDate :: Day -> Encoder
weekDate = encodeString . weekDateB

-- | Encodes ordinal date as a JSON string.
--
--   Format is @yyyy-ddd@.
ordinalDate :: Day -> Encoder
ordinalDate = encodeString . ordinalDateB

-- | Encodes 'TimeOfDay' as a JSON string.
--
--   Format is @hh-mm-ss[.sss]@.
timeOfDay :: TimeOfDay -> Encoder
timeOfDay = encodeString . timeOfDayB

-- | Encodes 'TimeZone' as a JSON string.
--
--   Format is @±hh:mm@.
timeZone :: TimeZone -> Encoder
timeZone = encodeString . timeZoneB

-- | Encodes 'UTCTime' as a JSON string.
--
--   Format is @yyyy-mm-ddThh-mm-ss[.sss]Z@.
utcTime :: UTCTime -> Encoder
utcTime = encodeString . utcTimeB

-- | Encodes 'LocalTime' as a JSON string.
--
--   Format is @yyyy-mm-ddThh-mm-ss[.sss]@.
localTime :: LocalTime -> Encoder
localTime = encodeString . localTimeB

-- | Encodes 'ZonedTime' as a JSON string.
--
--   Format is @yyyy-mm-ddThh-mm-ss[.sss]±hh:mm@.
zonedTime :: ZonedTime -> Encoder
zonedTime = encodeString . zonedTimeB



-- | Encodes 'UUID' as a JSON string.
--
--   Format is @uuuuuuuu-uuuu-uuuu-uuuu-uuuuuuuuuuuu@.
uuid :: UUID -> Encoder
uuid = encodeString . uuidB



-- | Encodes 'Word8' as a JSON number.
word8 :: Word8 -> Encoder
word8 = Encoder . word8Dec

-- | Encodes 'Word16' as a JSON number.
word16 :: Word16 -> Encoder
word16 = Encoder . word16Dec

-- | Encodes 'Word32' as a JSON number.
word32 :: Word32 -> Encoder
word32 = Encoder . word32Dec

-- | Encodes 'Word64' as a JSON number.
word64 :: Word64 -> Encoder
word64 = Encoder . word64Dec

-- | Encodes 'Word' as a JSON number.
word :: Word -> Encoder
word = Encoder . wordDec

-- | Encodes 'Natural' as a JSON number.
natural :: Natural -> Encoder
natural = Encoder . integerDec . fromIntegral



-- | Encodes 'Int8' as a JSON number.
int8 :: Int8 -> Encoder
int8 = Encoder . int8Dec

-- | Encodes 'Int16' as a JSON number.
int16 :: Int16 -> Encoder
int16 = Encoder . int16Dec

-- | Encodes 'Int32' as a JSON number.
int32 :: Int32 -> Encoder
int32 = Encoder . int32Dec

-- | Encodes 'Int64' as a JSON number.
int64 :: Int64 -> Encoder
int64 = Encoder . int64Dec

-- | Encodes 'Int' as a JSON number.
int :: Int -> Encoder
int = Encoder . intDec

-- | Encodes 'Integer' as a JSON number.
integer :: Integer -> Encoder
integer = Encoder . integerDec



-- | Encodes 'Float' as a JSON number.
--
--   @NaN@ and both infinities instead encode as a @null@.
float :: Float -> Encoder
float f = Encoder $ if isNaN f || isInfinite f
                      then nullB
                      else floatDec f

-- | Encodes 'Double' as a JSON number.
--
--   @NaN@ and both infinities instead encode as a @null@.
double :: Double -> Encoder
double d = Encoder $ if isNaN d || isInfinite d
                       then nullB
                       else doubleDec d

-- | Encodes 'Scientific' as a JSON number.
scientific :: Scientific -> Encoder
scientific = Encoder . scientificBuilder



-- | Encodes 'Bool' as a JSON boolean.
boolean :: Bool -> Encoder
boolean False = Encoder falseB
boolean True  = Encoder trueB



-- | Encodes a JSON null.
null :: Encoder
null = Encoder nullB
