{-# LANGUAGE BangPatterns
           , ExistentialQuantification
           , TemplateHaskellQuotes #-}

module Codec.JSON.Decoder.Array.TH
  ( ElementsQ
  , elementsQ
  ) where

import           Codec.JSON.Decoder.Array.Internal
import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Internal.TH

import           Language.Haskell.TH
import           Parser.Lathe



-- | Precompiled applicative array parser.
data ElementsQ a = forall x. ElementsMap (Code Q (x -> a)) (ElementsQ x)
                 | forall x. ElementsApply (ElementsQ (x -> a)) (ElementsQ x)
                 | ElementsOne (Code Q (Decoder a))

instance FunctorQ ElementsQ where
  fmapQ = ElementsMap

instance ApplyQ ElementsQ where
  (|*|) = ElementsApply

instance ElementQ ElementsQ where
  elementQ = ElementsOne



-- | Decode a JSON array as a non-empty tuple.
--
--   Fails if the JSON array holds a different number of elements.
elementsQ :: ElementsQ a -> Code Q (Decoder a)
elementsQ els =
  let go :: Word -> ElementsQ a -> Code Q (Path -> K -> Parser (Path, Error) a)
      go n xs =
        case xs of
          ElementsMap f b ->
            [|| \path k ->
                  $$(f) <$> $$(go n b) path k
             ||]

          ElementsApply g b ->
            [|| \path k -> do
                  f <- $$(go n g) path k
                  more <- nextElementP (Index path n)
                  if not more
                    then err (path, Malformed Benign expectsMore)
                    else do
                      k' <- nextValueP path
                      a <- $$(go (n + 1) b) path k'
                      pure (f a)
             ||]

          ElementsOne decQ ->
            [|| \path ->
                   runDecoder $$(decQ) (Index path n) EmptyBitmask
             ||]

  in [|| Decoder $ \path bits k0 ->
           case k0 of
             A -> do
               skipEndOr1
               mayK1 <- firstElementP path
               case mayK1 of
                 Nothing -> err (path, Malformed Benign expectsMore)
                 Just k1 -> do
                   a <- $$(go 0 els) path k1
                   more <- nextElementP path
                   if more
                     then err (path, Malformed Benign expectsFewer)
                     else pure a

             _ -> let !bits' = bits <> ArrayBit
                  in err (path, Mismatch bits' k0)
      ||]
