{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Encode.Boolean
  ( Test.JSON.Encode.Boolean.boolean
  ) where

import           Codec.Web.JSON.Decode as Decode
import           Codec.Web.JSON.Encode as Encode

import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



boolean :: Spec
boolean =
  describe "Boolean" $ do
    it "False" $ do
      decode Decode.boolean (encode $ Encode.boolean False) `shouldBe` Success "" False

    it "True" $ do
      decode Decode.boolean (encode $ Encode.boolean True) `shouldBe` Success "" True

