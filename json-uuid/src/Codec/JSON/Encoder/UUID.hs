{-# LANGUAGE DataKinds #-}

{-| Functions for encoding RFC 9562 UUIDs in RFC 8259 JSONs.
 -}

module Codec.JSON.Encoder.UUID
  ( uuid
  ) where

import           Codec.JSON.Encoder.Unsafe

import           Data.ByteString.Builder
import           Data.UUID.Types as UUID



-- | Encode a 'UUID' as a JSON string, in lower-case hexadecimal
--   @xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx@ format ("hex-and-dash"),
uuid :: UUID -> Encoder
uuid u = Encoder $ word8 0x22 <> byteString (UUID.toASCIIBytes u) <> word8 0x22
