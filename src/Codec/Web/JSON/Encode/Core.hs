{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving #-}

{-# OPTIONS_HADDOCK not-home #-}

{- | Access to encoder internals and convenience functions.
 -}

module Codec.Web.JSON.Encode.Core
  ( -- * Encoder
    Encoder (..)

    -- * Value
    -- ** Object
  , Name (..)
  , Pair (..)
  , EncoderPair (..)

    -- ** String
  , encodeString
  ) where

import           Data.ByteString.Builder
import           Data.String



-- | Plain JSON encoder.
newtype Encoder = Encoder { runEncoder :: Builder }
                  deriving newtype Show



-- | JSON object pair name.
newtype Name = Name Builder

instance IsString Name where
  fromString = Name . stringUtf8



-- | Generalized object pair builders.
class Pair a where
  -- | Encodes a pair with the given 'Name'.
  (.=) :: Name -> Encoder -> a

  -- | If an 'Encoder' is provided, encodes a pair with the given 'Name'.
  --   If no 'Encoder' is provided, behavior depends on the object encoder.
  (.=?) :: Name -> Maybe Encoder -> a

-- | Simple JSON object encoder.
data EncoderPair = EncoderPair Name (Maybe Encoder)

instance Pair EncoderPair where
  (.=) name = EncoderPair name . Just
  (.=?) name = EncoderPair name



{-# INLINE encodeString #-}
-- | Puts double quotes around a 'Builder'.
encodeString :: Builder -> Encoder
encodeString builder =
  Encoder $ word8 0x22 {- " -} <> builder <> word8 0x22 {- " -}
