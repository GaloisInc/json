{-# LANGUAGE BangPatterns #-}

{-| Functions for decoding RFC 9562 UUIDs in RFC 8259 JSONs.
 -}

module Codec.JSON.Decoder.UUID
  ( Codec.JSON.Decoder.UUID.uuid
  ) where

import           Codec.JSON.Decoder
import           Codec.JSON.Decoder.Unsafe
import           Codec.JSON.Decoder.String.Internal

import           Data.UUID.Types as UUID
import           Parser.Lathe as Lathe



-- | Decode a JSON string as a 'UUID', in hexadecimal
--   @xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx@ format ("hex-and-dash"). Case-insensitive.
uuid :: Decoder UUID
uuid =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             ro <- Lathe.byteString 36 (path, AbruptEnd)
             case UUID.fromASCIIBytes ro of
               Nothing ->
                 let malformed = "Expected UUID encoded as xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
                 in err (path, Malformed Benign malformed)
               Just u  -> do
                 end <- atEnd
                 if end
                   then pure u
                   else err (path, Malformed Benign "Found trailing data after the UUID")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)
