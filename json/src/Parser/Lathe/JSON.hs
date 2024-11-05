{-| Functions for parsing RFC 8259 JSONs.
 -}

module Parser.Lathe.JSON
  ( decoder
  , isWhitespace
  ) where

import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.Internal

import           Parser.Lathe



-- | Consume any number of bytes representing a JSON value.
--
--   Does not consume whitespace on either side of the value.
decoder :: Path -> Decoder a -> Parser (Path, Error) a
decoder path (Decoder one) = do
  k <- determineP path
  one path EmptyBitmask k
