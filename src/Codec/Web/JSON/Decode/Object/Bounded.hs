{-# LANGUAGE BangPatterns
           , ExistentialQuantification #-}

module Codec.Web.JSON.Decode.Object.Bounded
  ( BoundedPairs

  , boundedFail
  , boundedObject'
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



data BoundedMode a = forall x. Mandatory (Decoder x) (BoundedPairs (x -> a))
                   | forall x. Optional (Decoder (Maybe x)) (BoundedPairs (Maybe x -> a))

instance Functor BoundedMode where
  fmap f (Mandatory dec forth) = Mandatory dec $ (f .) <$> forth
  fmap f (Optional  dec forth) = Optional  dec $ (f .) <$> forth

-- | Branching 'Applicative' JSON object pair parser.
--
--   While not a valid 'Monad', this is a valid 'Selective'.
--
--   @ApplicativeDo@ and @RecordWildCards@ extensions may be helpful.
data BoundedPairs a = Pair Name (BoundedMode a)
                    | forall x. Select (BoundedPairs (Either x a)) (BoundedPairs (x -> a))
                    | Alt (BoundedPairs a) (BoundedPairs a)
                    | End a
                    | Err String

instance Functor BoundedPairs where
  fmap f (Pair name mode)     = Pair name $ f <$> mode
  fmap f (Select choose left) = Select (fmap f <$> choose) ((f .) <$> left)
  fmap f (Alt left right)     = Alt (f <$> left) (f <$> right)
  fmap f (End a)              = End $ f a
  fmap _ (Err msg)            = Err msg

instance Apply BoundedPairs where
  l@(Pair _ _) <.> Pair m b =
    Pair m $
      case b of
        Mandatory dec forth -> Mandatory dec $ liftF2 (.) l forth
        Optional  dec forth -> Optional  dec $ liftF2 (.) l forth

  l <.> r =
    case l of
      Pair m b             ->
        Pair m $
          case b of
            Mandatory dec forth -> Mandatory dec $ liftF2 flip forth r
            Optional  dec forth -> Optional  dec $ liftF2 flip forth r

      Select choose0 fill0 -> Select (liftF2 (\e x -> fmap ($ x) e) choose0 r)
                                     (liftF2 flip fill0 r)
      Alt left right       -> Alt (left <.> r) (right <.> r)
      End f                -> fmap f r
      Err msg              -> Err msg

instance Applicative BoundedPairs where
  pure = End

  (<*>) = (<.>)

instance Alt BoundedPairs where
  (<!>) = Alt

instance Selective BoundedPairs where
  select = Select

instance Pair BoundedPairs where
  {-# INLINE (.:) #-}
  Name name .: Decoder decoder =
    Pair (Name name) $ Mandatory (Decoder $ namePath name decoder) (End id)

  {-# INLINE (.:??) #-}
  Name name .:?? Decoder decoder =
    Pair (Name name) $ Optional (Decoder $ namePath name decoder) (End id)



packBoundedTree :: BoundedPairs a -> RadixTree (Maybe b) -> RadixTree (Maybe b)
packBoundedTree pair tree =
  case pair of
    Pair (Name name) mode ->
      case mode of
        Mandatory _ adv -> packBoundedTree adv $ Radix.insert name Nothing tree
        Optional  _ adv -> packBoundedTree adv $ Radix.insert name Nothing tree

    Select choose fill ->
      packBoundedTree choose $ packBoundedTree fill tree

    Alt left right ->
      packBoundedTree left $ packBoundedTree right tree

    End _ -> tree

    Err _ -> tree



parseBoundedObject
  :: DecodingHandler
  -> Int
  -> RadixTree (Maybe ByteString)
  -> Parser (RadixTree (Maybe ByteString))
parseBoundedObject handler chunklen = go
  where
    go tree = do
      mayResult <- tilNextNameP . withStringP $
                                    updateUtf8P handler tree
      case mayResult of
        Just (Nothing, back) -> do
          tilNextColonP
          copy <- copyValueP handler chunklen emptyCopy
          let !bs = commitCopy copy
              tree' = back $ Just bs
          again <- tilNextPairP
          if again
            then go tree'
            else pure tree'

        _ -> do
          tilNextColonP
          skipValueP handler
          again <- tilNextPairP
          if again
            then go tree
            else pure tree



moduleLoc :: String
moduleLoc = "json.Codec.Web.JSON.Decode.Object.Bounded: "

applyBoundedTree :: RadixTree (Maybe ByteString) -> BoundedPairs a -> Parser a
applyBoundedTree tree = go
  where
    go :: BoundedPairs a -> Parser a
    go pairs =
      case pairs of
        Pair (Name name) mode ->
          case mode of
            Mandatory dec forth ->
              case Radix.lookup name tree of
                Just (Just bs) -> do
                  a <- innerDecode dec bs
                  ($ a) <$> go forth

                Just Nothing -> err $ "No pair under name " <> shows name "]"

                Nothing -> errorWithoutStackTrace $
                             showString moduleLoc . ("No entry for mandatory name " <>)
                               $ showsPrec 11 name " in the radix tree"

            Optional dec forth ->
              case Radix.lookup name tree of
                Just (Just bs) -> do
                  a <- innerDecode dec bs
                  ($ a) <$> go forth

                Just Nothing -> ($ Nothing) <$> go forth

                Nothing -> errorWithoutStackTrace $
                             showString moduleLoc . ("No entry for optional name " <>)
                               $ showsPrec 11 name " in the radix tree"

        Select choose fill -> do
          ei <- go choose
          case ei of
            Right a -> pure a
            Left x  -> ($ x) <$> go fill

        Alt left right -> go left <|> go right

        End a -> pure a

        Err msg -> err msg



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



-- | Since 'BoundedPairs' is strong enough to be a 'Selective',
--   it's also strong enough to 'fail'.
boundedFail :: String -> BoundedPairs a
boundedFail str = Err str



-- | Parses a JSON object.
--
--   See 'Codec.Web.JSON.Decode.boundedObject' for operation details.
--
--   Values are copied and skipped using a custom 'DecodingHandler'. Copied values are
--   chunked every @Int@ decoded characters.
boundedObject' :: DecodingHandler -> Int -> BoundedPairs a -> Decoder a
boundedObject' handler chunklen pairs =
  withObject $ do
    has <- tilFirstPairP
    if has
      then do let tree = packBoundedTree pairs Radix.empty
              tree' <- parseBoundedObject handler chunklen tree
              applyBoundedTree tree' pairs

      else applyBoundedTree Radix.empty pairs
