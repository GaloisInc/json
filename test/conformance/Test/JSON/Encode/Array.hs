{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Encode.Array
  ( Test.JSON.Encode.Array.array
  ) where

import           Codec.Web.JSON.Decode as Decode
import           Codec.Web.JSON.Encode as Encode

import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



array :: Spec
array =
  it "Array" $
    let input = Encode.array
                  [ Encode.float 1.5
                  , Encode.text "data"
                  , Encode.boolean True
                  ]

        result = (1.5, "data", True)

        template = do
          a <- element $ \_ -> Decode.float
          b <- element $ \_ -> Decode.text
          c <- element $ \_ -> Decode.boolean
          pure (a, b, c)

    in decode (elementArray template) (encode input) `shouldBe` Success "" result
