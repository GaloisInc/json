{-# LANGUAGE BangPatterns #-}

{-| Functions for parsing JSON with incremental output.
 -}

module Codec.JSON.Decoder.Stream
  ( -- **Stream
    Stream (..)

    -- * Run
  , stream

    -- * Source
  , Source
  , mapSource
  , foldSource
  , foldSource'
  , sourceDecoder

    -- ** Object
    -- | === Key
  , KeyDecoder
  , stringKey
  , textKey
  , lazyTextKey
  , copyKey

    -- | === Sources
  , sourceObject
  , sourceObject_

    -- ** Array
  , sourceArray
  ) where

import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Stream.Internal
import           Codec.JSON.Decoder.String
import           Data.JSON.Internal

import           Data.Coerce
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as LT (Text)
import           Parser.Lathe



-- | Run a stream incrementally.
stream :: Source a r -> Blank -> Stream a Partial (Blank, Either (Path, Error) r)
stream (Source s) b =
  unfoldStream b $
    Effect $ do
      k <- nextValueP Root
      pure $ s Root EmptyBitmask k



-- | Apply a function to each element of the stream-decoder.
mapSource :: (a -> b) -> Source a r -> Source b r
mapSource f = \(Source source) ->
  Source $ \path bits k ->
    mapStream f (source path bits k)



-- | Left-associative fold of a stream-decoder, lazy in the accumulator.
foldSource
  :: (b -> a -> b)
  -> (b -> r -> b)
  -> b
  -> Source a r
  -> Decoder b
foldSource f g = \z (Source source) ->
  Decoder $ \path bits k ->
    foldStream f g z (source path bits k)




-- | Left-associative fold of a stream-decoder, strict in the accumulator.
foldSource'
  :: (b -> a -> b)
  -> (b -> r -> b)
  -> b
  -> Source a r
  -> Decoder b
foldSource' f g = \z (Source source) ->
  Decoder $ \path bits k ->
    foldStream' f g z (source path bits k)



-- | Use a non-incremental decoder as a single-element stream.
sourceDecoder :: Decoder a -> r -> Source a r
sourceDecoder (Decoder p) r =
  Source $ \path bits k ->
    Effect $
      (\a -> Yield a $ Return r) <$> p path bits k



-- | Decode a JSON object pair name as a 'String'.
stringKey :: KeyDecoder String
stringKey = KeyDecoder string'

-- | Decode a JSON object pair name as a strict 'T.Text'.
textKey :: KeyDecoder T.Text
textKey = KeyDecoder text'

-- | Decode a JSON object pair name as a lazy 'LT.Text'.
lazyTextKey :: KeyDecoder LT.Text
lazyTextKey = KeyDecoder lazyText'

-- | Get a copy of the underlying input alongside the object pair name.
copyKey :: KeyDecoder a -> KeyDecoder (JSONKey, a)
copyKey (KeyDecoder (Decoder parser)) =
  KeyDecoder $
    Decoder $ \path bits k ->
      coerce (match $ parser path bits k)



-- | Stream a JSON object. Object pair names are parsed with the given t'KeyDecoder'.
sourceObject :: KeyDecoder k -> (k -> r -> Source a r) -> r -> Source a r
sourceObject keyDecoder f r0 =
  Source $ \path bits k0 ->
    Effect $
      case k0 of
        O -> sourceObjectP path keyDecoder f r0
        _ -> let !bits' = bits <> ObjectBit
             in err (path, Mismatch bits' k0)



-- | Stream a JSON object. Object pair names are copied verbatim.
sourceObject_ :: (JSONKey -> r -> Source a r) -> r -> Source a r
sourceObject_ f r0 =
  Source $ \path bits k ->
    Effect $
      case k of
        O -> sourceObjectP_ path f r0
        _ -> let !bits' = bits <> ObjectBit
             in err (path, Mismatch bits' k)



-- | Stream a JSON array. The 'Word' argument is the element index, starts at @0@.
sourceArray :: (Word -> r -> Source a r) -> r -> Source a r
sourceArray f r0 =
  Source $ \path bits k0 ->
    Effect $
      case k0 of
        A -> sourceArrayP path f r0
        _ -> let !bits' = bits <> ArrayBit
             in err (path, Mismatch bits' k0)
