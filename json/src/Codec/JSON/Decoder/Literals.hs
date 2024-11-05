{-# LANGUAGE BangPatterns #-}

module Codec.JSON.Decoder.Literals
  ( bool
  , Codec.JSON.Decoder.Literals.maybe
  , Codec.JSON.Decoder.Literals.null
  ) where

import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Literals.Internal

import           Parser.Lathe



-- | Decode a JSON boolean into a 'Bool'.
bool :: Decoder Bool
bool =
  Decoder $ \path bits k ->
    case k of
      T -> True <$ trueP path
      F -> False <$ falseP path
      _ -> let !bits' = bits <> BooleanBit
           in err (path, Mismatch bits' k)



-- | Decode a JSON value into a 'Maybe', interpreting JSON null as 'Nothing'.
maybe :: Decoder a -> Decoder (Maybe a)
maybe one =
  Decoder $ \path bits k ->
    case k of
      X -> Nothing <$ nullP path
      _ -> Just <$> runDecoder one path (bits <> NullBit) k

-- | Decode a JSON null.
null :: Decoder ()
null =
  Decoder $ \path bits k ->
    case k of
      X -> nullP path
      _ -> let !bits' = bits <> NullBit
           in err (path, Mismatch bits' k)
