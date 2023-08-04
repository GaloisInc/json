{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.Boolean
  ( Test.JSON.Decode.Boolean.boolean
  ) where

import           Codec.Web.JSON.Decode as JSON

import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



boolean :: Spec
boolean =
  describe "Boolean" $ do
    it "False" $ do
      decode JSON.boolean "false" `shouldBe` Success "" False

    it "False (trail)" $ do
      decode JSON.boolean "falseA" `shouldBe` Success "A" False

    it "True" $ do
      decode JSON.boolean "true" `shouldBe` Success "" True

    it "True (trail)" $ do
      decode JSON.boolean "trueA" `shouldBe` Success "A" True
