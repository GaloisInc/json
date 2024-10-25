{-# LANGUAGE TemplateHaskellQuotes #-}

{-| Functions for precompiling decoders.
 -}

module Codec.JSON.Decoder.TH
  ( -- * Type classes
    -- ** Functor
    FunctorQ (..)
  , (|$|)

    -- ** Applicative
  , ApplyQ (..)
  , ApplicativeQ (..)

    -- * Decode
    -- ** Object
  , PairQ
  , (.|)
  , (?|)

    -- *** Applicative
  , PairsQ
  , pairsQ

    -- ** Array
  , ElementQ (..)

    -- *** Applicative
  , ElementsQ
  , elementsQ
  ) where

import           Codec.JSON.Decoder.Array.TH
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Internal.TH
import           Codec.JSON.Decoder.Literals (maybe)
import           Codec.JSON.Decoder.Object.TH

import           Data.Text (Text)
import           Language.Haskell.TH.Syntax



-- | Decode a JSON value at the given key.
--
--   Fails if the key is not present in the JSON object.
(.|) :: PairQ f => Text -> Code Q (Decoder a) -> f a
(.|) = pairQ

-- | Decode an optional JSON value at the given key.
--
--   Returns 'Nothing' if the key is not present in the JSON object or
--   if the value is a JSON null.
(?|) :: PairQ f => Text -> Code Q (Decoder a) -> f (Maybe a)
key ?| one = pairMaybeQ key [|| Codec.JSON.Decoder.Literals.maybe $$(one) ||]
