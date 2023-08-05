{-# LANGUAGE BangPatterns #-}

module Codec.Web.JSON.Decode.Array.Core where

import           Codec.Web.JSON.Decode.Core
import           Codec.Web.JSON.Parse.Array



-- | Parses a JSON array by applying the function to each element.
--
--   'Int' is the element index, starts at @0@.
array :: (Int -> Decoder a) -> Decoder [a]
array f =
  withArray $ do
    nonempty <- tilFirstValueP
    if nonempty
      then rest 0
      else pure []
  where
    rest !i = do
      a <- let Decoder decoder = f i
           in indexPath i decoder
      again <- tilNextValueP
      if again
        then (:) a <$> rest (i + 1)
        else pure [a]



-- | Parses a JSON array by folding through every element left-to-right.
--
--   'Int' is the element index, starts at @0@.
foldArray :: (b -> Int -> Decoder b) -> b -> Decoder b
foldArray f z =
  withArray $ do
    nonempty <- tilFirstValueP
    if nonempty
      then rest z 0
      else pure z
  where
    rest t !i = do
      t' <- let Decoder decoder = f t i
            in indexPath i decoder
      again <- tilNextValueP
      if again
        then rest t' (i + 1)
        else pure t'



-- | Streams a JSON array by folding through every element left-to-right.
--
--   'Int' is the element index, starts at @0@.
streamArray :: (b -> Int -> Stream a b) -> b -> Stream a b
streamArray f z =
  Parse $
    unDecoder $
      withArray $ do
        nonempty <- tilFirstValueP
        pure $ if nonempty
                 then go z 0
                 else Return z
  where
    go t !i = unroll $ f t i
      where
        unroll s =
          case s of
            Yield a s' -> Yield a $ unroll s'
            Parse e    -> Parse $ unroll <$> indexPath i e
            Return t'  -> Parse $ do
                            again <- tilNextValueP
                            pure $ if again
                                     then go t' (i + 1)
                                     else Return t'
