{-# OPTIONS_HADDOCK not-home #-}

{-| Data structure internals, helper operations and unsafe functions.
 -}

module Codec.JSON.Decoder.Unsafe
  ( -- * Itself
    Decoder (..)

  , Path (..)

  , Bitmask (..)

  , K (..)

  , Gravity (..)
  , Error (..)

    -- * Decode
    -- ** Object
  , radixTree
  , radixTree'
  , Pair (..)
  , PairQ (..)

    -- ** Array
  , snocList

    -- ** Number
  , bignum

    -- ** String
  , unsafeString
  , unsafeText
  , unsafeLazyText

    -- * Stream
  , Source (..)
  , KeyDecoder (..)
  ) where

import           Codec.JSON.Decoder.Array
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Internal.TH
import           Codec.JSON.Decoder.Number
import           Codec.JSON.Decoder.Object
import           Codec.JSON.Decoder.Stream.Internal
import           Codec.JSON.Decoder.String
