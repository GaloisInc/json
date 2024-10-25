module Codec.JSON.Decoder.Internal.TH
  ( FunctorQ (..)
  , (|$|)

  , ApplyQ (..)

  , ApplicativeQ (..)

  , PairQ (..)
  , ElementQ (..)
  ) where

import           Codec.JSON.Decoder.Internal

import           Data.Text (Text)
import           Language.Haskell.TH



-- | Variation of 'Functor' that uses typed splices.
class FunctorQ f where
  fmapQ :: Code Q (a -> b) -> f a -> f b

-- | Variation of '(<$>)' that uses typed splices.
infixl 4 |$|
(|$|) :: FunctorQ f => Code Q (a -> b) -> f a -> f b
(|$|) = fmapQ



-- | Variation of 'Data.Functor.Apply.Apply' that uses typed splices.
class FunctorQ f => ApplyQ f where
  infixl 4 |*|
  (|*|) :: f (a -> b) -> f a -> f b



-- | Variation of 'Applicative' that uses typed splices.
class ApplyQ f => ApplicativeQ f where
  pureQ :: Code Q a -> f a



-- | Class of precompiled JSON object parsers.
class PairQ f where
  -- | Ditto '(Codec.JSON.Decoder.TH..|)'.
  pairQ :: Text -> Code Q (Decoder a) -> f a

  -- | Decode an optional JSON value at the given key.
  --
  --   Returns 'Nothing' if the key is not present in the supplied JSON object.
  pairMaybeQ :: Text -> Code Q (Decoder (Maybe a)) -> f (Maybe a)



-- | Class of precompiled JSON array parsers.
class ElementQ f where
  -- | Decode a JSON value at the current position.
  elementQ :: Code Q (Decoder a) -> f a
