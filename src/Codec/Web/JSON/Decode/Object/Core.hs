module Codec.Web.JSON.Decode.Object.Core where

import           Codec.Web.JSON.Decode.Core
import           Codec.Web.JSON.Parse.Object



-- | Parses a JSON object by folding through every pair left-to-right.
foldObject :: FoldName n -> (b -> n -> Decoder b) -> b -> Decoder b
foldObject (FoldName (Decoder nameDecoder) convert) f z =
  withObject $ do
    has <- tilFirstPairP
    if has
      then rest z
      else pure z
  where
    rest t = do
      name <- tilNextNameP nameDecoder
      tilNextColonP
      t' <- let Decoder decoder = f t name
            in namePath (convert name) decoder
      again <- tilNextPairP
      if again
        then rest t'
        else pure t'



-- | Streams a JSON object by folding through every pair left-to-right.
streamObject :: FoldName n -> (b -> n -> Stream a b) -> b -> Stream a b
streamObject (FoldName (Decoder nameDecoder) convert) f z =
  Parse $
    unDecoder $
      withObject $ do
        has <- tilFirstPairP
        if has
          then go z
          else pure $ Return z
  where
    go t = do
      name <- tilNextNameP nameDecoder

      let unroll s =
            case s of
              Yield a s' -> pure . Yield a . Parse $ unroll s'
              Parse e    -> namePath (convert name) e >>= unroll
              Return t'  -> do again <- tilNextPairP
                               if again
                                 then go t'
                                 else pure $ Return t'

      tilNextColonP
      unroll $ f t name
