{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.Null
  ( Test.JSON.Decode.Null.null
  ) where

import           Codec.Web.JSON.Decode as JSON

import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



null :: Spec
null =
  describe "Null" $ do
    it "No trail" $ do
      decode JSON.null "null" `shouldBe` Success "" ()

    it "Trail" $ do
      decode JSON.null "nullA" `shouldBe` Success "A" ()
