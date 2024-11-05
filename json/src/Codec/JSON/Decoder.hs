{-# LANGUAGE BangPatterns #-}

{-| Pure incremental RFC 8259 JSON decoder.

    == Correctness

    All functions that parse or validate JSON object pair names treat surrogate Unicode
    code points as invalid input.

    == Laziness

    Decoder functions that return concrete types in this library fully evaluate
    their results before returning unless their documentation specifies otherwise.
 -}

module Codec.JSON.Decoder
  ( -- * Itself
    Decoder
  , mapEither

    -- * Run
  , ByteOffset
  , More (..)
  , Scrap (..)

  , Path (..)
  , Error

  , decode

    -- * Decode

    -- ** Object
  , Pair
  , (.:)
  , (?:)

    -- *** Applicative
  , PairsA
  , pairsA

    -- *** Monad
  , PairsM
  , pairsM

    -- ** Array
  , list
  , Element (..)

    -- *** Applicative
  , ElementsA
  , elementsA

    -- ** Number
    -- | === Unsigned integral
  , Codec.JSON.Decoder.Number.word8
  , word16
  , word32
  , word64
  , word

    -- | === Signed integral
  , Codec.JSON.Decoder.Number.int8
  , int16
  , int32
  , int64
  , int

    -- | === Floating-point
    --
    --   Note that JSON numbers cannot represent @NaN@, \(+\infty\) and \(-\infty\).
  , float
  , double

    -- ** String
  , string
  , text
  , lazyText

    -- | === Eager-failure variants
  , string'
  , text'
  , lazyText'

    -- ** Boolean
  , bool

    -- ** Null
  , Codec.JSON.Decoder.Literals.null
  , Codec.JSON.Decoder.Literals.maybe

    -- ** Raw
  , JSON
  , copy
  , json
  , jsonObject
  , jsonArray
  , jsonNumber
  , jsonString
  , jsonBoolean
  , jsonNull

    -- | === Strict Unicode conformance variants
  , json'
  , jsonObject'
  , jsonArray'
  , jsonString'
  ) where

import           Codec.JSON.Decoder.Array
import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.JSON
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Literals
import           Codec.JSON.Decoder.Number
import           Codec.JSON.Decoder.Object
import           Codec.JSON.Decoder.String
import           Data.JSON.Internal

import qualified Data.ByteString.Lazy as LB
import           Data.Coerce
import           Data.Text (Text)
import           Parser.Lathe



-- | Transform a parsed value or fail in place.
mapEither :: (a -> Either String b) -> Decoder a -> Decoder b
mapEither f (Decoder d) = do
  Decoder $ \path bits k -> do
    a <- d path bits k
    case f a of
      Right b -> pure b
      Left e  -> err (path, Malformed Benign e)



badTrail :: String
badTrail = "Unexpected trailing data"



-- | Run a decoder by providing all the input immediately.
--
--   Fails if there is any trailing data remaining after parsing.
decode :: Decoder a -> LB.ByteString -> (Scrap, Either (Path, Error) a)
decode (Decoder parser) =
  parse $ do
    k <- nextValueP Root
    parser Root EmptyBitmask k <* endOfInputP (Root, Malformed Fatal badTrail)



-- | Decode a JSON value at the given key.
--
--   Fails if the key is not present in the JSON object.
(.:) :: Pair f => Text -> Decoder a -> f a
(.:) = pair

-- | Decode an optional JSON value at the given key.
--
--   Returns 'Nothing' if the key is not present in the JSON object or
--   if the value is a JSON null.
(?:) :: Pair f => Text -> Decoder a -> f (Maybe a)
key ?: parser = pairMaybe key (Codec.JSON.Decoder.Literals.maybe parser)



-- | Get a copy of the underlying input alongside the decoding result.
copy :: Decoder a -> Decoder (JSON, a)
copy (Decoder parser) =
  Decoder $ \path bits k ->
    coerce (match $ parser path bits k)
