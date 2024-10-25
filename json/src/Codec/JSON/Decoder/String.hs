{-# LANGUAGE BangPatterns #-}

module Codec.JSON.Decoder.String
  ( string
  , string'
  , unsafeString

  , text
  , text'
  , unsafeText

  , lazyText
  , lazyText'
  , unsafeLazyText

  , stringified
  ) where

import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.String.Internal

import           Data.Text (Text)
import qualified Data.Text.Internal.Lazy as LT (Text (..), defaultChunkSize)
import qualified Data.Text.Lazy as LT
import           Parser.Lathe



-- | Decode a JSON string as a 'String'.
--
--   Surrogate Unicode code points are replaced with @U+FFFD REPLACEMENT CHARACTER@.
string :: Decoder String
string = string_ Replace

-- | Decode a JSON string as a 'String'.
--
--   Surrogate Unicode code points are treated as invalid input.
string' :: Decoder String
string' = string_ Complain

-- | Decode a JSON string as a 'String'.
--
--   Surrogate Unicode code points are treated as valid scalar values.
unsafeString :: Decoder String
unsafeString = string_ Preserve

{-# INLINE string_ #-}
string_ :: SurrogateHandling -> Decoder String
string_ handl =
  Decoder $ \path bits k ->
    case k of
      S -> do skipEndOr1
              stringP handl path
      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as a strict 'Text'.
--
--   Surrogate Unicode code points are replaced with @U+FFFD REPLACEMENT CHARACTER@.
text :: Decoder Text
text = text_ Replace

-- | Decode a JSON text as a strict 'Text'.
--
--   Surrogate Unicode code points are treated as invalid input.
text' :: Decoder Text
text' = text_ Complain

-- | Decode a JSON text as a strict 'Text'.
--
--   Surrogate Unicode code points are treated as valid scalar values.
unsafeText :: Decoder Text
unsafeText = text_ Preserve

{-# INLINE text_ #-}
text_ :: SurrogateHandling -> Decoder Text
text_ handl = do
  Decoder $ \path bits k ->
    case k of
      S -> do skipEndOr1
              t <- lazyTextP handl path
              pure $! LT.toStrict t
      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as a lazy 'LT.Text'.
--
--   Surrogate Unicode code points are replaced with @U+FFFD REPLACEMENT CHARACTER@.
lazyText :: Decoder LT.Text
lazyText = lazyText_ Replace

-- | Decode a JSON string as a lazy 'LT.Text'.
--
--   Surrogate Unicode code points are treated as invalid input.
lazyText' :: Decoder LT.Text
lazyText' = lazyText_ Complain

-- | Decode a JSON string as a lazy 'LT.Text'.
--
--   Surrogate Unicode code points are treated as valid scalar values.
unsafeLazyText :: Decoder LT.Text
unsafeLazyText = lazyText_ Preserve

{-# INLINE lazyText_ #-}
lazyText_ :: SurrogateHandling -> Decoder LT.Text
lazyText_ handl = do
  Decoder $ \path bits k ->
    case k of
      S -> do skipEndOr1
              lazyTextP handl path
      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



lazyTextP :: SurrogateHandling -> Path -> Parser (Path, Error) LT.Text
lazyTextP handl path = go id
  where
    go cs = do
      chunk <- chunkP handl path LT.defaultChunkSize
      case chunk of
        Chunk more c ->
          case more of
            More -> go (cs . LT.Chunk c)
            End  -> let !ll = LT.Chunk c LT.empty
                    in pure $! cs ll

        Nil -> pure $! cs LT.empty



-- | Decode a JSON value embedded as a JSON string.
stringified :: Decoder a -> Decoder a
stringified (Decoder parser) =
  Decoder $ \path bits k ->
    case k of
      S -> embedP LT.defaultChunkSize path $ do
             k' <- nextValueP path
             let str = "Unexpected trailing data in a stringified JSON"
             parser path EmptyBitmask k' <* endOfInputP (path, Malformed Fatal str)
      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)
