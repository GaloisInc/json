{-# OPTIONS_HADDOCK not-home #-}

{-| Data strucutre internals.
 -}

module Codec.JSON.Encoder.Unsafe
  ( -- * Itself
    Encoder (..)

    -- * Produce
    -- ** Object
  , Object1 (..)

    -- | === Key
  , KeyEncoder (..)

    -- ** Array
  , Array1 (..)
  ) where

import           Codec.JSON.Encoder.Internal
