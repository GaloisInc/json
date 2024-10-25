{-# LANGUAGE BangPatterns
           , ExistentialQuantification
           , FlexibleInstances #-}

module Codec.JSON.Decoder.Array
  ( list
  , snocList

  , ElementsA
  , elementsA
  ) where

import           Codec.JSON.Decoder.Array.Internal
import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.Internal

import           Data.Functor.Apply
import           Parser.Lathe



-- | Decode a JSON array as a list of values.
list :: Decoder a -> Decoder [a]
list decoder =
  Decoder $ \path bits k ->
    case k of
      A -> do
        (_, as) <- snocList_ decoder path
        pure $! reverse as

      _ -> let !bits' = bits <> ArrayBit
           in err (path, Mismatch bits' k)

-- | Decode a JSON array as a snoc-list of values, alongside its length.
snocList :: Decoder a -> Decoder (Word, [a])
snocList decoder =
  Decoder $ \path bits k ->
    case k of
      A -> snocList_ decoder path
      _ -> let !bits' = bits <> ArrayBit
           in err (path, Mismatch bits' k)


snocList_ :: Decoder a -> Path -> Parser (Path, Error) (Word, [a])
snocList_ decoder path = do
  skipEndOr1
  mayK' <- firstElementP path
  case mayK' of
    Nothing -> pure (0, [])
    Just k' -> go [] 0 k'
  where
    go as !n !k = do
      a <- runDecoder decoder (Index path n) EmptyBitmask k
      more <- nextElementP path
      let !n' = n + 1
      if more
        then do
          k' <- nextValueP path
          go (a:as) n' k'

        else pure (n', a:as)



-- | Applicative array parser.
data ElementsA a = forall x. ElementsMap (x -> a) (ElementsA x)
                 | forall x. ElementsApply (ElementsA (x -> a)) (ElementsA x)
                 | ElementsOne (Decoder a)

instance Functor ElementsA where
  fmap = ElementsMap

instance Apply ElementsA where
  (<.>) = ElementsApply

instance Element ElementsA where
  element = ElementsOne



-- | Decode a JSON array as a non-empty tuple.
--
--   Fails if the JSON array holds a different number of elements.
elementsA :: ElementsA a -> Decoder a
elementsA els =
  Decoder $ \path bits k0 ->
    case k0 of
      A -> do
        skipEndOr1
        mayK1 <- firstElementP path
        case mayK1 of
          Nothing -> err (path, Malformed Benign expectsMore)
          Just k1 -> do
            a <- go 0 k1 els
            more <- nextElementP path
            if more
              then err (path, Malformed Benign expectsFewer)
              else pure a
            where
              go :: Word -> K -> ElementsA a -> Parser (Path, Error) a
              go !n !k xs =
                case xs of
                  ElementsMap f b -> f <$> go n k b
                  ElementsApply g b -> do
                    f <- go n k g
                    more <- nextElementP (Index path n)
                    if not more
                      then err (path, Malformed Benign expectsMore)
                      else do
                        k' <- nextValueP path
                        a <- go (n + 1) k' b
                        pure (f a)

                  ElementsOne (Decoder p) -> p (Index path n) EmptyBitmask k

      _ -> let !bits' = bits <> ArrayBit
           in err (path, Mismatch bits' k0)
