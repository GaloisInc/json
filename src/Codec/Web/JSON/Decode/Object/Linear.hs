{-# LANGUAGE ExistentialQuantification #-}

module Codec.Web.JSON.Decode.Object.Linear
  ( LinearPairs

  , linearObject'
  ) where

import           Codec.Web.JSON.Decode.Core
import           Codec.Web.JSON.Parse.Object
import           Codec.Web.JSON.Parse.String
import           Codec.Web.JSON.Parse.Knot
import           Data.Attoparsec.Error
import           Data.RadixTree.Char (RadixTree)
import qualified Data.RadixTree.Char as Radix
import           Encoding.Mixed.Error

import           Data.Attoparsec.ByteString hiding (Result)
import           Data.Functor.Alt
import           Unsafe.Coerce



data LinearMode a = forall x. Mandatory (Decoder x) (LinearPairs (x -> a))
                  | forall x. Optional (Decoder (Maybe x)) (LinearPairs (Maybe x -> a))

instance Functor LinearMode where
  fmap f (Mandatory dec forth) = Mandatory dec $ (f .) <$> forth
  fmap f (Optional  dec forth) = Optional  dec $ (f .) <$> forth

-- | Non-branching 'Applicative' JSON object pair parser.
--
--   @ApplicativeDo@ and @RecordWildCards@ extensions may be helpful.
data LinearPairs a = Pair Name (LinearMode a)
                   | End a

instance Functor LinearPairs where
  fmap f (Pair name mode) = Pair name $ f <$> mode
  fmap f (End a)          = End $ f a

instance Apply LinearPairs where
  l <.> Pair m b =
    case l of
      Pair _ _ ->
        Pair m $
          case b of
            Mandatory dec forth -> Mandatory dec $ liftF2 (.) l forth
            Optional  dec forth -> Optional  dec $ liftF2 (.) l forth

      End f    -> Pair m $ f <$> b

  l <.> End a = ($ a) <$> l

instance Applicative LinearPairs where
  pure = End

  (<*>) = (<.>)

instance Pair LinearPairs where
  {-# INLINE (.:) #-}
  Name name .: Decoder decoder =
    Pair (Name name) $ Mandatory (Decoder $ namePath name decoder) (End id)

  {-# INLINE (.:??) #-}
  Name name .:?? Decoder decoder =
    Pair (Name name) $ Optional (Decoder $ namePath name decoder) (End id)



data Hidden

data Result = Untouched (Decoder Hidden)
            | Parsed Hidden

packLinearTree :: LinearPairs a -> RadixTree Result -> Either Name (RadixTree Result)
packLinearTree (Pair (Name name) mode) tree =
  let hide :: Decoder x -> Decoder Hidden
      hide = unsafeCoerce

      ~(someDecoder, advance) =
        case mode of
          Mandatory decoder f -> (Untouched $ hide decoder, packLinearTree f tree)
          Optional  decoder f -> (Untouched $ hide decoder, packLinearTree f tree)

  in case advance of
       Right g  ->
         case Radix.insertUnique name someDecoder g of
           Just tree' -> Right tree'
           Nothing    -> Left $ Name name

       Left e -> Left e

packLinearTree (End _) tree = Right tree



parseLinearObject :: DecodingHandler -> RadixTree Result -> Parser (RadixTree Result)
parseLinearObject handler = go
  where
    go tree = do
      mayResult <- tilNextNameP . withStringP $
                                    updateUtf8P handler tree
      case mayResult of
        Just (Untouched decoder, back) -> do
          tilNextColonP
          x <- unDecoder decoder
          let tree' = back $ Parsed x
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



applyLinearTree :: RadixTree Result -> LinearPairs a -> Either Name a
applyLinearTree tree = go
  where
    go (Pair (Name utf) mode) =
      case mode of
        Mandatory _ f ->
          case Radix.dirtyLookup utf tree of
            Parsed x    -> ($ (unsafeCoerce :: Hidden -> x) x) <$> applyLinearTree tree f
            Untouched _ -> Left (Name utf)

        Optional _ f ->
          case Radix.dirtyLookup utf tree of
            Parsed x    ->
              ($ (unsafeCoerce :: Hidden -> Maybe x) x) <$> applyLinearTree tree f

            Untouched _ -> ($ Nothing) <$> applyLinearTree tree f

    go (End a) = Right a



-- | Parses a JSON object.
--
--   See 'Codec.Web.JSON.Decode.linearObject' for operation details.
--
--   Values are skipped using a custom 'DecodingHandler'.
linearObject' :: DecodingHandler -> LinearPairs a -> Decoder a
linearObject' handler pairs =
  withObject $ do
    has <- tilFirstPairP
    if has
      then case packLinearTree pairs Radix.empty of
             Right tree -> do
               tree' <- parseLinearObject handler tree
               case applyLinearTree tree' pairs of
                 Right a          -> pure a
                 Left (Name name) -> err $ "No pair under name " <> shows name ""

             Left (Name name) ->
               err $ "Object parser mentions name " <> shows name " twice"

      else case applyLinearTree Radix.empty pairs of
             Right a          -> pure a
             Left (Name name) -> err $ "No pair under name " <> shows name ""
