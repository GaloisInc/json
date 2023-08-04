{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving
           , RankNTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

{- | Access to decoder internals and convenience functions.
 -}

module Codec.Web.JSON.Decode.Core
  ( -- * Decoder

    -- | Unlike 'Parser', which operates over individual bytes and is thus
    --   a proper 'Monad', a 'Decoder' always consumes either one (and only one) value
    --   or a collection of values delimited by separators.
    --
    --   Care must thus be taken to ensure the underlying 'Parser' on success always
    --   consumes the entirety of the value and not a byte more.
    Decoder (..)

    -- ** Stream

    -- | Same caveats as those for 'Decoder' apply on 'Stream' completion as well.
  , Stream (..)

    -- ** Path

    -- | JSON path is internally carried through 'Parser'\'s context and only wrapped
    --   into a 'Path' after decoding concludes.
  , Path (..)
  , namePath
  , indexPath

    -- * Value
  , V (..)
  , vname
  , expectedFound
  , withValue

    -- ** Object
  , withObject

    -- *** Pair
  , Name (..)
  , Pair (..)

    -- *** Fold
  , FoldName (..)

    -- ** Array
  , withArray

    -- ** String
  , withString

    -- ** Number
  , withNumber

    -- ** Boolean
  , withBoolean

    -- ** Null
  , withNull
  ) where

import           Codec.Web.JSON.Decode.Stream
import           Codec.Web.JSON.Parse.Knot

import           Control.Applicative
import           Data.Attoparsec.ByteString as Atto
import           Data.Functor.Alt
import           Data.String



-- | Dot-notated
--   [JSONPath](https://www.ietf.org/archive/id/draft-goessner-dispatch-jsonpath-00.html).
newtype Path = Path String
               deriving newtype Show

-- | @namePath key@ appends @.key@ to the current path.
namePath :: String -> Parser a -> Parser a
namePath str = (<?> ('.':str))

-- | @indexPath n@ appends @[n]@ to the current path.
indexPath :: Int -> Parser a -> Parser a
indexPath n = (<?> ('[':shows n "]"))



-- | Plain JSON decoder.
newtype Decoder a = Decoder { unDecoder :: Parser a }
                    deriving newtype Functor

-- | '(<!>)' relies on parser backtracking: all the input required to parse the right
--   alternative will be kept until the left one completes.
instance Alt Decoder where
  Decoder a <!> Decoder b = Decoder $ a <|> b



{-# INLINE withValue #-}
-- | Skips whitespace until the first byte of a value is encountered. That byte is
--   not consumed.
withValue :: (V -> Parser a) -> Decoder a
withValue = Decoder . withValueP



{-# INLINE withObject #-}
-- | Skips whitespace until an object value is encountered. Consumes the left curly
--   bracket.
--
--   Encountering any other value results in a failure.
withObject :: Parser a -> Decoder a
withObject = Decoder . withObjectP



-- | JSON object pair name.
newtype Name = -- | The 'String' is used lazily by the library.
               Name String
               deriving newtype (Show, IsString)


-- | Generalized object pair decoders.
class Pair f where
  -- | Decodes a value from a pair with the given 'Name'.
  --
  --   If no such 'Name' is present in the object, the underlying object parser fails.
  --   If the value is @null@, the behavior depends on the 'Decoder'.
  (.:) :: Name -> Decoder a -> f a

  -- | Decodes a value from a pair with the given 'Name'.
  --
  --   If no such 'Name' is present in the object, returns a 'Nothing'.
  --   If the value is @null@, the behavior depends on the 'Decoder'.
  (.:??) :: Name -> Decoder (Maybe a) -> f (Maybe a)



-- | JSON object pair name decoder.
data FoldName a = FoldName
                    (Decoder a)   -- ^ This allows decoding any JSON value in name
                                  --   position, not just a string.

                    (a -> String) -- ^ Converter to append name to 'Path'.



{-# INLINE withArray #-}
-- | Skips whitespace until an array value is encountered. Consumes the left square
--   bracket.
--
--   Encountering any other value results in a failure.
withArray :: Parser a -> Decoder a
withArray = Decoder . withArrayP


{-# INLINE withString #-}
-- | Skips whitespace until a string value is encountered. Consumes the left double quote.
--
--   Encountering any other value results in a failure.
withString :: Parser a -> Decoder a
withString = Decoder . withStringP


{-# INLINE withNumber #-}
-- | Skips whitespace until a number value is encountered. The first byte of the number
--   is not consumed.
--
--   Encountering any other value results in a failure.
withNumber :: Parser a -> Decoder a
withNumber = Decoder . withNumberP


{-# INLINE withBoolean #-}
-- | Skips whitespace until a boolean value is encountered. The first byte of either
--   boolean value is consumed.
--
--   Encountering any other value results in a failure.
withBoolean :: (Bool -> Parser a) -> Decoder a
withBoolean = Decoder . withBooleanP


{-# INLINE withNull #-}
-- | Skips whitespace until a null value is encountered. Consumes the @n@.
--
--   Encountering any other value results in a failure.
withNull :: Parser a -> Decoder a
withNull = Decoder . withNullP
