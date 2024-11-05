{-# LANGUAGE BangPatterns
           , RankNTypes
           , UnboxedTuples #-}

{-# OPTIONS_HADDOCK hide #-}

module Codec.JSON.Decoder.Stream.Internal
  ( Stream (..)
  , chainStream
  , unfoldStream
  , mapStream
  , foldStream
  , foldStream'

  , Source (..)
  , sourceObjectP
  , sourceObjectP_
  , sourceArrayP
  ) where

import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.JSON.Internal
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.String.Internal
import           Data.JSON.Internal

import           Parser.Lathe



-- | A list parametrized by an element type @a@, an  effect type @m@
--   and the return type @r@.
data Stream a m r = Yield a (Stream a m r)
                  | Effect (m (Stream a m r))
                  | Return r

instance Functor m => Functor (Stream a m) where
  fmap f = go
    where
      go s =
        case s of
          Yield a s' -> Yield a (go s')
          Effect m   -> Effect (go <$> m)
          Return r   -> Return (f r)



chainStream
  :: Stream a (Parser e) r
  -> (r -> Stream a (Parser e) o)
  -> Stream a (Parser e) o
chainStream x next = go x
  where
    go s =
      case s of
        Yield a s' -> Yield a (go s')
        Effect one -> Effect (go <$> one)
        Return r   -> next r


unfoldStream
  :: Blank
  -> Stream a (Parser (Path, Error)) r
  -> Stream a Partial (Blank, Either (Path, Error) r)
unfoldStream b s =
  case s of
    Yield a s' -> Yield a (unfoldStream b s')
    Effect m   -> Effect $ go (draw m b)
      where
        go x =
          case x of
            Partial re    -> Partial $ \supply -> go (re supply)
            Done (b', ei) ->
              let !(# r #) = case ei of
                               Right s' -> (# unfoldStream b' s' #)
                               Left e   -> (# Return (b', Left e) #)

              in Done r

    Return r   -> Return (b, Right r)



mapStream :: Functor m => (a -> b) -> Stream a m r -> Stream b m r
mapStream f = go
  where
    go s =
      case s of
        Yield a s' -> Yield (f a) $ go s'
        Effect m   -> Effect (go <$> m)
        Return r   -> Return r



foldStream
  :: (b -> a -> b)
  -> (b -> r -> b)
  -> b
  -> Stream a (Parser (Path, Error)) r
  -> Parser (Path, Error) b
foldStream f g = go
  where
    go z s =
      case s of
        Yield a s' -> go (f z a) s'
        Effect m   -> go z =<< m
        Return r   -> pure (g z r)


foldStream'
  :: (b -> a -> b)
  -> (b -> r -> b)
  -> b
  -> Stream a (Parser (Path, Error)) r
  -> Parser (Path, Error) b
foldStream' f g = go
  where
    go !z s =
      case s of
        Yield a s' -> let !z' = f z a in go z' s'
        Effect m   -> go z =<< m
        Return r   -> pure $! g z r



-- | Decoder type for streams.
newtype Source a r =
          Source
            { runSource :: Path -> Bitmask -> K -> Stream a (Parser (Path, Error)) r }

instance Functor (Source a) where
  fmap f (Source source) =
    Source $ \path bits k ->
      fmap f $ source path bits k



sourceObjectP
  :: Path
  -> KeyDecoder k
  -> (k -> r -> Source a r)
  -> r
  -> Parser (Path, Error) (Stream a (Parser (Path, Error)) r)
sourceObjectP path (KeyDecoder (Decoder keyP)) f r0 = do
  skipEndOr1
  nonempty <- firstKeyP path
  if nonempty
    then go r0
    else pure $ Return r0
      where
        go r = do
          nextKeyP path
          (raw, key) <- match $ keyP path EmptyBitmask S
          nextColonP path
          k' <- nextValueP (Key path (JSONKey (JSON raw)))
          pure $
            chainStream
              (runSource (f key r) (Key path (JSONKey (JSON raw))) EmptyBitmask k')
              ( \r' ->
                  Effect $ do
                    more <- nextPairP path
                    if more
                      then go r'
                      else pure (Return r')
              )



sourceObjectP_
  :: Path
  -> (JSONKey -> r -> Source a r)
  -> r
  -> Parser (Path, Error) (Stream a (Parser (Path, Error)) r)
sourceObjectP_ path f r0 = do
  skipEndOr1
  nonempty <- firstKeyP path
  if nonempty
    then go r0
    else pure $ Return r0
      where
        go r = do
          nextKeyP path
          (raw, _) <- match $ jsonStringP Complain_ path
          let key = JSONKey (JSON raw)
          nextColonP path
          k' <- nextValueP (Key path (JSONKey (JSON raw)))
          pure $
            chainStream
              (runSource (f key r) (Key path key) EmptyBitmask k')
              ( \r' ->
                  Effect $ do
                    more <- nextPairP path
                    if more
                      then do
                        nextKeyP path
                        go r'

                      else pure (Return r')
              )



sourceArrayP
  :: Path
  -> (Word -> r -> Source a r)
  -> r
  -> Parser (Path, Error) (Stream a (Parser (Path, Error)) r)
sourceArrayP path f r0 = do
  skipEndOr1
  mayK1 <- firstElementP path
  case mayK1 of
    Nothing -> pure $ Return r0
    Just k1 -> go 0 k1 r0
      where
        go !n !k r =
          pure $
            chainStream
              (runSource (f n r) (Index path n) EmptyBitmask k)
              ( \r' ->
                  Effect $ do
                    more <- nextElementP path
                    if more
                      then do
                        k' <- nextValueP path
                        go (n + 1) k' r'

                      else pure (Return r')
              )
