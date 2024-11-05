{-# LANGUAGE BangPatterns
           , GADTs
           , ScopedTypeVariables
           , UnboxedTuples #-}

module Codec.JSON.Decoder.Object.Internal
  ( lookupText
  , insertText

  , Qualifier (..)

  , MalformedParser (..)
  , State (..)
  , parsePairsA
  ) where

import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.JSON.Internal
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.String.Internal
import           Data.JSON.Internal

import           Control.Exception
import           Data.RadixTree.Word8.Strict (RadixTree (..))
import qualified Data.RadixTree.Word8.Strict as Radix
import qualified Data.Radix1Tree.Word8.Key.Unsafe as Radix1 (unsafeFeedText)
import           Data.Radix1Tree.Word8.Strict (Radix1Tree)
import           Data.Radix1Tree.Word8.Strict.Zipper (Context1)
import qualified Data.Radix1Tree.Word8.Strict.Zipper as Radix1
import           Data.Text (Text)
import qualified Data.Text.Internal.Lazy as LT (chunkOverhead)
import           GHC.Exts (Any)
import           Parser.Lathe



lookupText :: Text -> RadixTree a -> Maybe a
lookupText txt = Radix.lookup (Radix.feedText txt)

insertText :: Text -> a -> RadixTree a -> RadixTree a
insertText txt raw = Radix.insert (Radix.feedText txt) raw

descendText1 :: Text -> Either (Radix1Tree a) (Context1 a) -> Maybe (Context1 a)
descendText1 txt = Radix1.descend (Radix1.unsafeFeedText txt)



data Qualifier a where
  Mandatory :: Qualifier        a
  Optional  :: Qualifier (Maybe a)



data MalformedParser = MalformedParser

instance Show MalformedParser where
  showsPrec _ MalformedParser =
    showString "Malformed parser"

instance Exception MalformedParser



data State = Unsaturated !(Decoder Any)
           | Saturated !Any

parsePairsA :: Path -> RadixTree State -> Parser (Path, Error) (RadixTree State)
parsePairsA path r0 = do
  skipEndOr1
  nonempty <- firstKeyP path
  if nonempty
    then go r0
    else pure r0
  where
    hit = chunkP Complain path (128 - LT.chunkOverhead)

    drill (RadixTree mx t) = do
      skipEndOr1
      chunk0 <- hit
      case chunk0 of
        Nil ->
          case mx of
            Just (Unsaturated dec) -> pure $ Just (dec, \y -> RadixTree (Just y) t)
            _                      -> pure Nothing

        Chunk more0 txt0 -> do
          drill1 more0 $ descendText1 txt0 (Left t)
      where
        drill1 !more mayCtx =
          case mayCtx of
            Nothing   ->
              Nothing <$ case more of
                           More -> stringP_ Complain_ path
                           End  -> pure ()
            Just !ctx ->
              case more of
                More -> do
                  chunk <- hit
                  case chunk of
                    Chunk more' txt -> drill1 more' $ descendText1 txt (Right ctx)
                    Nil             -> stop ctx

                End -> stop ctx

        stop ctx =
          case Radix1.focus ctx of
            Just (Unsaturated dec, reinsert) ->
              pure $ Just (dec, \y -> RadixTree mx $ reinsert y)

            _ -> pure Nothing


    go !r = do
      (key, mayDec) <- match $ drill r

      let rawKey = JSONKey (JSON key)

      nextColonP path
      k <- nextValueP path

      r' <- case mayDec of
              Nothing              -> r <$ jsonP Preserve_ (Key path rawKey) k
              Just (dec, reinsert) -> do
                a <- runDecoder dec path EmptyBitmask k
                pure $! reinsert (Saturated a)

      more <- nextPairP path
      if more
        then do
          nextKeyP path
          go r'

        else pure r'
