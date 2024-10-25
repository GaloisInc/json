{-# LANGUAGE OverloadedStrings #-}

module Test.JSON.Decoder.Stream
  ( Test.JSON.Decoder.Stream.stream
  ) where

import           Codec.JSON.Decoder as JSON
import           Codec.JSON.Decoder.Stream
import           Data.JSON as JSON

import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Text (Text)
import           Test.Hspec



arrayarray :: LB.ByteString
arrayarray =
  "[[1,null,3e-1],[true,\"yes\",{ }],[[ ],0.0e0,\"\"]]"

arrayarrayspace :: LB.ByteString
arrayarrayspace =
  " [ [ 1 , null , 3e-1 ] , [ true , \"yes\" , { } ] , [ [ ] , 0.0e0 , \"\" ] ] "

arrayarrayelements :: [(Word, Word, LB.ByteString)]
arrayarrayelements =
  [ (0, 0, "1"   ), (0, 1, "null"   ), (0, 2, "3e-1")
  , (1, 0, "true"), (1, 1, "\"yes\""), (1, 2, "{ }")
  , (2, 0, "[ ]" ), (2, 1, "0.0e0"  ), (2, 2, "\"\"")
  ]



objectobject :: LB.ByteString
objectobject =
  "{\"A\":{\"a\":1,\"b\":null,\"c\":3e-1}\
  \,\"B\":{\"a\":true,\"b\":\"yes\",\"c\":{ }}\
  \,\"C\":{\"a\":{ },\"b\":0.0e0,\"c\":\"\"}}"

objectobjectspace :: LB.ByteString
objectobjectspace =
  " { \"A\" : { \"a\" : 1    , \"b\" : null    , \"c\" : 3e-1 } \
  \ , \"B\" : { \"a\" : true , \"b\" : \"yes\" , \"c\" : { }  } \
  \ , \"C\" : { \"a\" : { }  , \"b\" : 0.0e0   , \"c\" : \"\" } } "

objectobjectelements :: [(Text, Text, LB.ByteString)]
objectobjectelements =
  [ ("A", "a", "1"   ), ("A", "b", "null"   ), ("A", "c", "3e-1")
  , ("B", "a", "true"), ("B", "b", "\"yes\""), ("B", "c", "{ }")
  , ("C", "a", "{ }" ), ("C", "b", "0.0e0"  ), ("C", "c", "\"\"")
  ]

objectobjectelements_ :: [(LB.ByteString, LB.ByteString, LB.ByteString)]
objectobjectelements_ =
  [ ("\"A\"", "\"a\"", "1"   ), ("\"A\"", "\"b\"", "null"   ), ("\"A\"", "\"c\"", "3e-1")
  , ("\"B\"", "\"a\"", "true"), ("\"B\"", "\"b\"", "\"yes\""), ("\"B\"", "\"c\"", "{ }")
  , ("\"C\"", "\"a\"", "{ }" ), ("\"C\"", "\"b\"", "0.0e0"  ), ("\"C\"", "\"c\"", "\"\"")
  ]




foldObjectOfObjects :: Decoder [(Text, Text, LB.ByteString)]
foldObjectOfObjects =
  fmap ($ []) $
    foldSource'
      (\acc x -> acc . (:) x) const id $
         sourceObject textKey
           (\i -> sourceObject textKey
                    (\j r -> mapSource (\x -> (i, j, JSON.getRaw x)) $
                               sourceDecoder JSON.json r
                    )
           )
           ()

foldObjectOfObjects_ :: Decoder [(LB.ByteString, LB.ByteString, LB.ByteString)]
foldObjectOfObjects_ =
  fmap ($ []) $
    foldSource'
      (\acc x -> acc . (:) x) const id $
         sourceObject_
           (\i -> sourceObject_
                    (\j r -> mapSource (\x -> ( JSON.getRaw $ JSON.unKey i
                                              , JSON.getRaw $ JSON.unKey j
                                              , JSON.getRaw x
                                              )
                                       )
                               $ sourceDecoder JSON.json r
                    )
           )
           ()

foldArrayOfArrays :: Decoder [(Word, Word, LB.ByteString)]
foldArrayOfArrays =
  fmap ($ []) $
    foldSource'
      (\acc x -> acc . (:) x) const id $
         sourceArray
           (\i -> sourceArray
                    (\j r -> mapSource (\x -> (i, j, JSON.getRaw x)) $
                               sourceDecoder JSON.json r
                    )
           )
           ()


stream :: Spec
stream = do
  describe "sourceObject" $ do
    it (LB.unpack objectobject) $
      let (_, ei) = decode foldObjectOfObjects objectobject
      in case ei of
           Left err -> fail $ show err
           Right xs -> xs `shouldBe` objectobjectelements

    it (LB.unpack objectobjectspace) $
      let (_, ei) = decode foldObjectOfObjects objectobjectspace
      in case ei of
           Left err -> fail $ show err
           Right xs -> xs `shouldBe` objectobjectelements

  describe "sourceObject_" $ do
    it (LB.unpack objectobject) $
      let (_, ei) = decode foldObjectOfObjects_ objectobject
      in case ei of
           Left err -> fail $ show err
           Right xs -> xs `shouldBe` objectobjectelements_

    it (LB.unpack objectobjectspace) $
      let (_, ei) = decode foldObjectOfObjects_ objectobjectspace
      in case ei of
           Left err -> fail $ show err
           Right xs -> xs `shouldBe` objectobjectelements_

  describe "sourceArray" $ do
    it (LB.unpack arrayarray) $
      let (_, ei) = decode foldArrayOfArrays arrayarray
      in case ei of
           Left err -> fail $ show err
           Right xs -> xs `shouldBe` arrayarrayelements

    it (LB.unpack arrayarrayspace) $
      let (_, ei) = decode foldArrayOfArrays arrayarrayspace
      in case ei of
           Left err -> fail $ show err
           Right xs -> xs `shouldBe` arrayarrayelements
