{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.Decoders where

import           Codec.Web.JSON.Decode

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.IORef
import           Data.Monoid (Endo (..))
import           Data.Functor.Identity
import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



unsource :: Monad m => Source m r a b -> m (Result r ([a], b))
unsource = go mempty
  where
    go as s =
      case s of
        Step a s'    -> go (as <> Endo (a:)) s'
        Effect e     -> e >>= go as
        Done r b     -> pure $ Success r (appEndo as [], b)
        Failed r p m -> pure $ Failure r p m



chunks :: [ByteString]
chunks = [ " [ 1, {}, 2.5, tr", "ue, \"data\", [", "], false", ", null ]A" ]

malformed :: [ByteString]
malformed = [ " [ 1, {}, 2.5, tr", "ue, \"data\", [", "] ? false", ", null ]A" ]

chunksIO :: IORef [ByteString] -> IO ByteString
chunksIO ref = do
  cs <- readIORef ref
  case cs of
    c:ds -> do writeIORef ref ds
               pure c

    []   -> pure BS.empty

result :: [BSLC.ByteString]
result = ["1", " {}", " 2.5", " true", " \"data\"", " []", " false", " null"]



decoders :: Spec
decoders =
  describe "Decoders" $ do
    describe "decode" $ do
      it "Valid" $
        decode (array $ \_ -> rawValue) (BSLC.fromChunks chunks)
          `shouldBe` Success "A" result

      it "Malformed" $
        decode (array $ \_ -> rawValue) (BSLC.fromChunks malformed)
          `shouldSatisfy` \f -> case f of
                                  Failure "? false, null ]A" (Path "$") _ -> True
                                  _                                       -> False

    describe "decodeM" $ do
      it "Valid" $ do
        ref <- newIORef chunks
        decodeM (array $ \_ -> rawValue) (chunksIO ref)
          `shouldReturn` Success "A" result

      it "Malformed" $ do
        ref <- newIORef malformed
        output <- decodeM (array $ \_ -> rawValue) (chunksIO ref)
        output `shouldSatisfy` \f -> case f of
                                       Failure "? false" (Path "$") _ -> True
                                       _                              -> False

    describe "source" $ do
      it "Valid" $ do
        unsource ( source
                     (streamArray (\i _ -> stream $ (\a -> (a, i + 1)) <$> rawValue) 0)
                     (BSLC.fromChunks chunks)
                 )
          `shouldBe` Identity (Success "A" (result, 8 :: Int))

      it "Malformed" $ do
        unsource (
          source
            (streamArray (\i _ -> stream $ (\a -> (a, i + 1)) <$> rawValue) (0 :: Int))
            (BSLC.fromChunks malformed)
                 )
          `shouldSatisfy` \f ->
                            case f of
                              Identity (Failure "? false, null ]A" (Path "$") _) -> True
                              _                                                  -> False

    describe "sourceF" $ do
      it "Valid" $ do
        ref <- newIORef chunks
        unsource ( sourceF
                     (streamArray (\i _ -> stream $ (\a -> (a, i + 1)) <$> rawValue) 0)
                     (chunksIO ref)
                 )
          `shouldReturn` Success "A" (result, 8 :: Int)

      it "Malformed" $ do
        ref <- newIORef malformed
        output <-
          unsource $
            sourceF
              (streamArray (\i _ -> stream $ (\a -> (a, i + 1)) <$> rawValue) (0 :: Int))
              (chunksIO ref)
        output `shouldSatisfy` \f -> case f of
                                       Failure "? false" (Path "$") _ -> True
                                       _                              -> False
