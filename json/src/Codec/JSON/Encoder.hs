{-| Pure incremental RFC 8259 JSON encoder.

    Encoding functions in this module produce JSON without insignificant whitespace.
 -}

module Codec.JSON.Encoder
  ( -- * Itself
    Encoder

    -- * Run
  , encode

    -- * Encode
    -- ** Object
  , KeyEncoder
  , stringKey
  , textKey
  , lazyTextKey
  , jsonKey

    -- *** Semigroup
  , emptyObject
  , object1
  , Object1
  , pair

    -- *** List
  , Pair
  , (.=)
  , (?=)
  , pairs

    -- ** Array
    -- *** Semigroup
  , emptyArray
  , array1
  , Array1
  , element

    -- *** List
  , list
  , nonEmpty

    -- ** Number
    -- | === Unsigned integer
  , word8
  , word16
  , word32
  , word64
  , word
  , natural

    -- | === Signed integer
  , int8
  , int16
  , int32
  , int64
  , int
  , integer

    -- | === Floating-point
  , float
  , double

    -- ** String
  , string
  , text
  , lazyText

    -- ** Boolean
  , bool

    -- ** Null
  , Codec.JSON.Encoder.maybe
  , Codec.JSON.Encoder.Internal.null

    -- ** Raw
  , JSON
  , json
  ) where

import           Codec.JSON.Encoder.Internal
import           Data.JSON
import           Data.JSON.Internal

import           Data.Coerce
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT



-- | Use a JSON object pair name and a JSON value as a JSON object pair.
(.=) :: KeyEncoder -> Encoder -> Pair
key .= val = Pair . Just $ pair key val

-- | Use a JSON object pair name and an optional JSON value as a JSON object pair.
--
--   Providing 'Nothing' excludes the pair from the resulting object.
(?=) :: KeyEncoder -> Maybe Encoder -> Pair
key ?= val = Pair $ pair key <$> val

-- | Encode a list of t'Pair's as a JSON object.
pairs :: [Pair] -> Encoder
pairs ps =
  case catMaybes $ coerce ps of
    []   -> emptyObject
    p:qs -> object1 $ sconcat (p :| qs)



-- | Encode a 'String' as a JSON object pair name.
stringKey :: String -> KeyEncoder
stringKey = KeyEncoder . string

-- | Encode a strict 'Text' as a JSON object pair name.
textKey :: Text -> KeyEncoder
textKey = KeyEncoder . text

-- | Encode a lazy 'Text' as a JSON object pair name.
lazyTextKey :: LT.Text -> KeyEncoder
lazyTextKey = KeyEncoder . lazyText

-- | Use a raw object pair name as a JSON object pair name.
jsonKey :: JSONKey -> KeyEncoder
jsonKey = \(JSONKey ro) -> KeyEncoder (json ro)



-- | Encode a list of encoders as a JSON array.
list :: [Encoder] -> Encoder
list    []  = emptyArray
list (a:as) = array1 $ list1 a as

-- | Encode a non-empty list of encoders as a JSON array.
nonEmpty :: NonEmpty Encoder -> Encoder
nonEmpty (a :| as) = array1 $ list1 a as

list1 :: Encoder -> [Encoder] -> Array1
list1 a as =
  case as of
    []   -> element a
    b:bs -> element a <> list1 b bs



-- | Encode a 'Bool' as a JSON boolean.
bool :: Bool -> Encoder
bool True  = true
bool False = false



-- | Encode a 'Maybe', using 'Codec.JSON.Encoder.null' as 'Nothing'.
maybe :: Maybe Encoder -> Encoder
maybe (Just (Encoder a)) = Encoder a
maybe Nothing            = coerce Codec.JSON.Encoder.Internal.null
