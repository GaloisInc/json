{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.Array
  ( Test.JSON.Decode.Array.array
  ) where

import           Codec.Web.JSON.Decode as JSON

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Identity
import           Data.Monoid (Endo (..))
import           Data.String
import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



plain :: IsString a => (a -> [ByteString] -> Expectation) -> Spec
plain f = do
  it "Empty" $
    f "[]"
      []

  it "Basic" $
    f " \r\n\t [  \r\n\t {\"foo\":1.5, \"bar\":[]}  \r\n\t, \t\r[   \r\n\t 1 \
      \\r\n\t],false,true,null]"
      [ "{\"foo\":1.5, \"bar\":[]}", " \t\r[   \r\n\t 1 \r\n\t]", "false", "true", "null" ]



unsource :: Source Identity r a b -> Result r ([a], b)
unsource = go mempty
  where
    go as s =
      case s of
        Step a s'    -> go (as <> Endo (a:)) s'
        Effect e     -> go as $ runIdentity e
        Done r b     -> Success r (appEndo as [], b)
        Failed r p m -> Failure r p m



array :: Spec
array =
  describe "Array" $ do
    describe "Array" $ do
      describe "No trail" $
        plain $ \a b ->
          decode (JSON.array $ \_ -> rawValue) a `shouldBe` Success "" b

      describe "Trail" $
        plain $ \a b ->
          decode (JSON.array $ \_ -> rawValue) (a <> "A") `shouldBe` Success "A" b

    describe "Raw" $ do
      describe "No trail" $
        plain $ \a _ ->
          decode rawArray a `shouldBe` Success "" a

      describe "Trail" $
        plain $ \a _ ->
          decode rawArray (a <> "A") `shouldBe` Success "A" a

    describe "Fold" $ do
      let asArray = flip appEndo [] <$>
                      foldArray (\b n -> (\a -> b <> Endo ((n, a):)) <$> rawValue) mempty

      describe "No trail" $
        plain $ \a b ->
          decode asArray a `shouldBe` Success "" (zip [0..] b)

      describe "Trail" $
        plain $ \a b ->
          decode asArray (a <> "A") `shouldBe` Success "A" (zip [0..] b)

    describe "Stream" $ do
      let input = streamArray (\() n -> stream ((\a -> ((n, a), ())) <$> rawValue)) ()

      describe "No trail" $
        plain $ \a b ->
          unsource (source input a) `shouldBe` Success "" (zip [0..] b, ())

      describe "Trail" $
        plain $ \a b ->
          unsource (source input $ a <> "A") `shouldBe` Success "A" (zip [0..] b, ())

    describe "Elements" $ do
      let raw = "[1.5, true, \"data\"]"

          result = (1.5, True, "data")

          template = do
            a <- element $ \_ -> float
            b <- element $ \_ -> boolean
            c <- element $ \_ -> text
            d <- mayElement $ \_ -> JSON.null
            case d of
              Nothing -> pure (a, b, c)
              Just _  -> fail "Fourth element found"

      it "No trail" $
        decode (elementArray template) raw `shouldBe` Success "" result

      it "Trail" $
        decode (elementArray template) (raw <> "A") `shouldBe` Success "A" result
