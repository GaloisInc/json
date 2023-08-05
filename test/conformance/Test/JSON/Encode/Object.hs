{-# LANGUAGE ApplicativeDo
           , OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Encode.Object
  ( Test.JSON.Encode.Object.object
  ) where

import           Codec.Web.JSON.Decode as Decode
import           Codec.Web.JSON.Encode as Encode

import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



object :: Spec
object =
  it "Object" $
    let input = Encode.object
                  [ "foo"                       .= Encode.float 1.5
                  , Encode.textUtf16Name "𝄞 ♭𝅘𝅥" .= Encode.text "data"
                  , Encode.textName "🃊🃉🃈🃇🃆"     .= Encode.boolean True
                  ]

        result = (1.5, "data", True)

        template = do
          a <- "foo"   .: Decode.float
          b <- "𝄞 ♭𝅘𝅥"  .: Decode.text
          c <- "🃊🃉🃈🃇🃆" .: Decode.boolean
          pure (a, b, c)

    in decode (linearObject template) (encode input) `shouldBe` Success "" result
