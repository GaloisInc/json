{-# LANGUAGE OverloadedStrings
           , RankNTypes
           , ScopedTypeVariables
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.Object
  ( Test.JSON.Decode.Object.object
  ) where

import           Codec.Web.JSON.Decode as JSON

import           Control.Selective (Selective, ifS)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Identity
import           Data.Monoid (Endo (..))
import           Data.String
import           Data.Text (Text)
import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



applicative
  :: (Applicative f, Pair f)
  => ( ByteString
    -> (Float, Maybe Text, Bool, Maybe Int)
    -> f (Float, Maybe Text, Bool, Maybe Int)
    -> a
     )
  -> a
applicative f =
  f "{\"foo\":1.5, \"bar\":\"data\", \"baz\":true, \"other\":{}}"
    (1.5, Just "data", True, Nothing)
    $ (,,,) <$> "foo"  .:  float
            <*> "bar"  .:? text
            <*> "baz"  .:  boolean
            <*> "none" .:? int



choose
  :: (Selective f, Pair f)
  => (forall x. String -> f x)
  -> (ByteString -> (Float, Text) -> f (Float, Text) -> a)
  -> a
choose brick f =
  f "{\"foo\":1.5, \"bar\":\"data\", \"baz\":true, \"other\":{}}"
    (1.5, "data")
    $ (,) <$> "foo" .: float
          <*> ifS ("baz" .: boolean)
                ("bar" .: text)
                (brick "NOPE")



monadic
  :: (MonadFail f, Pair f)
  => (ByteString -> (Float, Text) -> f (Float, Text) -> a) -> a
monadic f =
  f "{\"foo\":1.5, \"bar\":\"data\", \"baz\":true, \"other\":{}}"
    (1.5, "data")
    $ do foo <- "foo" .: float
         baz <- "baz" .: boolean
         if baz
           then (,) foo <$> "bar" .: text
           else fail "NOPE"



plain :: IsString a => (a -> [(ByteString, ByteString)] -> Expectation) -> Spec
plain f = do
  it "Empty" $
    f "{}"
      []

  it "Basic" $
    f " \r\n { \t \"foo\" \r\n: \t1.5 \t , \"bar\":\"data\",\"baz\":true\t,\r\"other\":{}}"
      [ ("\"foo\"", " \t1.5")
      , ("\"bar\"", "\"data\"")
      , ("\"baz\"", "true")
      , ("\"other\"", "{}")
      ]



unsource :: Source Identity r a b -> Result r ([a], b)
unsource = go mempty
  where
    go as s =
      case s of
        Step a s'    -> go (as <> Endo (a:)) s'
        Effect e     -> go as $ runIdentity e
        Done r b     -> Success r (appEndo as [], b)
        Failed r p m -> Failure r p m



object :: Spec
object =
  describe "Object" $ do
    describe "Linear" $
      it "Applicative" $
        applicative $ \input result template ->
          decode (linearObject template) input `shouldBe` Success "" result

    describe "Bounded" $ do
      it "Applicative" $
        applicative $ \input result template ->
          decode (boundedObject template) input `shouldBe` Success "" result

      it "Selective" $
        choose boundedFail $ \input result template ->
          decode (boundedObject template) input `shouldBe` Success "" result

    describe "Plain" $ do
      it "Applicative" $
        applicative $ \input result template ->
          decode (JSON.object template) input `shouldBe` Success "" result

      it "Selective" $
        choose fail $ \input result template ->
          decode (JSON.object template) input `shouldBe` Success "" result

      it "MonadFail" $
        monadic $ \input result template ->
          decode (JSON.object template) input `shouldBe` Success "" result

    describe "Skip" $ do
      describe "No trail" $
        plain $ \a _ ->
          decode skipObject a `shouldBe` Success "" ()

      describe "Trail" $
        plain $ \a _ ->
          decode skipObject (a <> "A") `shouldBe` Success "A" ()


    describe "Raw" $ do
      describe "No trail" $
        plain $ \a _ ->
          decode rawObject a `shouldBe` Success "" a

      describe "Trail" $
        plain $ \a _ ->
          decode rawObject (a <> "A") `shouldBe` Success "A" a

    describe "Fold" $ do
      let asObject =
            flip appEndo [] <$>
              foldObject rawFoldName
                         (\b n -> (\a -> b <> Endo ((n, a):)) <$> rawValue) mempty

      describe "No trail" $
        plain $ \a b ->
          decode asObject a `shouldBe` Success "" b

      describe "Trail" $
        plain $ \a b ->
          decode asObject (a <> "A") `shouldBe` Success "A" b

    describe "Stream" $ do
      let input = streamObject rawFoldName
                    (\() n -> stream $ (\a -> ((n, a), ())) <$> rawValue) ()

      describe "No trail" $
        plain $ \a b ->
          unsource (source input a) `shouldBe` Success "" (b, ())

      describe "Trail" $
        plain $ \a b ->
          unsource (source input $ a <> "A") `shouldBe` Success "A" (b, ())
