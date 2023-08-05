module Codec.Web.JSON.Decode.Array.Elements
  ( Elements

  , element
  , mayElement
  , elementArray
  ) where

import           Codec.Web.JSON.Decode.Core
import           Codec.Web.JSON.Parse.Array
import           Data.Attoparsec.Error

import           Control.Applicative ((<|>))
import           Control.Selective
import           Data.Attoparsec.ByteString
import           Data.Functor.Alt
import           Data.Functor.Bind



-- | Sequential JSON array parser.
newtype Elements a = Elements { unElements :: Bool -> Int -> Parser (a, Bool, Int) }

instance Functor Elements where
  {-# INLINE fmap #-}
  fmap f (Elements elements) =
    Elements $ \more n ->
      (\ (a, more', n') -> (f a, more', n') ) <$> elements more n

instance Apply Elements where
  {-# INLINE (<.>) #-}
  Elements g <.> Elements b =
    Elements $ \more0 n0 -> do
      (f, more1, n1) <- g more0 n0
      (a, more2, n2) <- b more1 n1
      pure (f a, more2, n2)

instance Applicative Elements where
  {-# INLINE pure #-}
  pure a = Elements $ \more n -> pure (a, more, n)

  {-# INLINE (<*>) #-}
  (<*>) = (<.>)

instance Bind Elements where
  {-# INLINE (>>-) #-}
  Elements b >>- m =
    Elements $ \more0 n0 -> do
      (a, more1, n1) <- b more0 n0
      unElements (m a) more1 n1

instance Monad Elements where
  {-# INLINE (>>=) #-}
  (>>=) = (>>-)

instance Selective Elements where
  {-# INLINE select #-}
  select = selectM

instance MonadFail Elements where
  {-# INLINE fail #-}
  fail msg = Elements $ \_ _ -> err msg

-- | '(<!>)' relies on parser backtracking: all the input required to parse the right
--   alternative will be kept until the left one completes.
instance Alt Elements where
  {-# INLINE (<!>) #-}
  Elements l <!> Elements r =
    Elements $ \more n -> l more n <|> r more n



-- | Parses the next array element.
--
--   Fails if the end of the array has already been reached.
--
--   'Int' is the element index, starts at @0@.
element :: (Int -> Decoder a) -> Elements a
element f =
  Elements $ \more n ->
    if more
      then do
        let Decoder decoder = f n
        a <- indexPath n decoder
        again <- tilNextValueP
        pure (a, again, n + 1)

      else err $ "Expected array to have more than " <> shows n " elements"



-- | Parses the next array element.
--
--   Returns 'Nothing' if the end of the array has already been reached.
--
--   'Int' is the element index, starts at @0@.
mayElement :: (Int -> Decoder a) -> Elements (Maybe a)
mayElement f =
  Elements $ \more n ->
    if more
      then do
        let Decoder decoder = f n
        a <- indexPath n decoder
        again <- tilNextValueP
        pure (Just a, again, n + 1)

      else pure (Nothing, False, n)



-- | Parses a JSON array.
--
--   Fails if the array still has unconsumed elements after 'Elements' resolve.
elementArray :: Elements a -> Decoder a
elementArray elements =
  withArray $ do
    more <- tilFirstValueP
    (a, more', n) <- unElements elements more 0
    if more'
      then err $ "Expected array to have only " <> shows n " elements"
      else pure a
