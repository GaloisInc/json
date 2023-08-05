{-# LANGUAGE BangPatterns #-}

module Codec.Web.JSON.Decode.Object.Plain
  ( Pairs

  , object'
  ) where

import           Codec.Web.JSON.Decode.Core
import           Codec.Web.JSON.Parse.Object
import           Codec.Web.JSON.Parse.String
import           Codec.Web.JSON.Parse.Knot
import           Data.Attoparsec.Error
import           Data.ByteString.Lazy.Copy
import           Data.RadixTree.Char (RadixTree)
import qualified Data.RadixTree.Char as Radix
import           Encoding.Mixed.Error

import           Control.Applicative ((<|>))
import           Control.Selective
import           Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy hiding (copy)
import           Data.ByteString.Lazy.Internal (ByteString (..))
import           Data.Functor.Alt
import           Data.Functor.Bind



-- | Simple 'Monad'ic JSON object pair parser.
newtype Pairs a = Pairs { unPairs :: RadixTree ByteString -> Parser a }

instance Functor Pairs where
  {-# INLINE fmap #-}
  fmap f (Pairs pairs) = Pairs $ \tree -> f <$> pairs tree

instance Apply Pairs where
  {-# INLINE (<.>) #-}
  Pairs f <.> Pairs b = Pairs $ \tree -> f tree <*> b tree

instance Applicative Pairs where
  {-# INLINE pure #-}
  pure a = Pairs $ \_ -> pure a

  {-# INLINE (<*>) #-}
  (<*>) = (<.>)

instance Bind Pairs where
  {-# INLINE (>>-) #-}
  Pairs b >>- m =
    Pairs $ \tree -> do
      a <- b tree
      unPairs (m a) tree

instance Monad Pairs where
  {-# INLINE (>>=) #-}
  (>>=) = (>>-)

instance Selective Pairs where
  {-# INLINE select #-}
  select = selectM

instance MonadFail Pairs where
  {-# INLINE fail #-}
  fail str = Pairs $ \_ -> err str

instance Alt Pairs where
  {-# INLINE (<!>) #-}
  Pairs l <!> Pairs r = Pairs $ \tree -> l tree <|> r tree

instance Pair Pairs where
  {-# INLINE (.:) #-}
  Name name .: decoder =
    Pairs $ \tree ->
      case Radix.lookup name tree of
        Just bs -> namePath name $ innerDecode decoder bs
        Nothing -> err $ "No pair under name " <> shows name ""

  {-# INLINE (.:??) #-}
  Name name .:?? decoder =
    Pairs $ \tree ->
      case Radix.lookup name tree of
        Just bs -> namePath name $ innerDecode decoder bs
        Nothing -> pure Nothing



innerDecode :: Decoder a -> ByteString -> Parser a
innerDecode (Decoder decoder) bs0 =
  case bs0 of
    Chunk c cs -> go cs $ parse decoder c
    Empty      -> go empty $ parse decoder BS.empty
  where
    go bs result =
      case result of
        Done _ a        -> pure a
        Atto.Fail _ c e -> err e <?> mconcat c
        Atto.Partial f  -> case bs of
                             Chunk c cs -> go cs (f c)
                             Empty      -> go empty (f BS.empty)



-- | Parses a JSON object.
--
--   See 'Codec.Web.JSON.Decode.object' for operation details.
--
--   Values are copied using a custom 'DecodingHandler' and
--   chunked every @Int@ decoded characters.
object' :: DecodingHandler -> Int -> Pairs a -> Decoder a
object' handler chunklen (Pairs pairs) =
  withObject $ do
    has <- tilFirstPairP
    if has
      then do tree <- go Radix.empty
              pairs tree

      else pairs Radix.empty
  where
    go tree = do
      (mayResult, back) <- tilNextNameP . withStringP $
                                            alterUtf8P handler tree
      case mayResult of
        Nothing -> do
          tilNextColonP

          copy <- copyValueP handler chunklen emptyCopy
          let !bs = commitCopy copy
              tree' = back bs

          again <- tilNextPairP
          if again
            then go tree'
            else pure tree'

        Just _ -> do
          tilNextColonP
          skipValueP handler
          again <- tilNextPairP
          if again
            then go tree
            else pure tree
