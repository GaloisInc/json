{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.Number
  ( number
  ) where

import           Codec.Web.JSON.Decode

import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.String
import           Test.Hspec


deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



isFailure :: Result r a -> Bool
isFailure (Failure _ _ _) = True
isFailure _               = False



integral :: (Eq a, Num a, Show a) => Decoder a -> Spec
integral decoder = do
  it "-0" $ do
    decode decoder "-0" `shouldBe` Success "" 0

  it "12" $ do
    decode decoder "12" `shouldBe` Success "" 12

  it "3.4e1" $ do
    decode decoder "34" `shouldBe` Success "" 34

  it "0.056e+3" $ do
    decode decoder "56" `shouldBe` Success "" 56

  it "7800e-2" $ do
    decode decoder "78" `shouldBe` Success "" 78

  it "15A" $ do
    decode decoder "15A" `shouldBe` Success "A" 15

  it "0.28e2A" $ do
    decode decoder "0.28e2A" `shouldBe` Success "A" 28

  it "4900e-2A" $ do
    decode decoder "4900e-2A" `shouldBe` Success "A" 49



unsignedIntegral :: (Eq a, Num a, Show a) => Decoder a -> Spec
unsignedIntegral decoder = do
  it "-1" $ do
    decode decoder "-1" `shouldSatisfy` \r -> case r of
                                                Failure "1" _ _ -> True
                                                _               -> False



signedIntegral :: (Eq a, Num a, Show a) => Decoder a -> Spec
signedIntegral decoder = do
  it "-82A" $ do
    decode decoder "-82A" `shouldBe` Success "A" (-82)

  it "-7.4e1A" $ do
    decode decoder "-7.4e1A" `shouldBe` Success "A" (-74)

  it "-0.109e+3A" $ do
    decode decoder "-0.109e+3A" `shouldBe` Success "A" (-109)

  it "-5500e-2A" $ do
    decode decoder "-5500e-2A" `shouldBe` Success "A" (-55)



bsInt :: Integral a => (Integer -> Integer) -> a -> BSLC.ByteString
bsInt f n = BSLC.pack . show . f $ fromIntegral n



integralEdges :: forall a. (Bounded a, Eq a, Integral a, Show a) => Decoder a -> Spec
integralEdges decoder = do
  it "maxBound" $ do
    decode decoder (bsInt id (maxBound :: a)) `shouldBe` Success "" (maxBound :: a)

  it "maxBound + 1" $ do
    decode decoder (bsInt (+1) (maxBound :: a)) `shouldSatisfy` isFailure

  it "(maxBound + 1) * 10" $ do
    decode decoder (bsInt ((*10) . (+1)) (maxBound :: a)) `shouldSatisfy` isFailure

  it "minBound" $ do
    decode decoder (bsInt id (minBound :: a)) `shouldBe` Success "" (minBound :: a)

  it "minBound - 1" $ do
    decode decoder (bsInt (subtract 1) (minBound :: a)) `shouldSatisfy` isFailure

  it "(minBound + 1) * 10" $ do
    decode decoder (bsInt ((*10) . subtract 1) (minBound :: a)) `shouldSatisfy` isFailure



unsignedLargeIntegral :: (Eq a, Num a, Show a) => Decoder a -> Spec
unsignedLargeIntegral decoder = do
  it "Large" $ do
    decode decoder "12345678912345678912345678912345678912345678912345678912345"
      `shouldBe` Success "" 12345678912345678912345678912345678912345678912345678912345

signedLargeIntegral :: (Eq a, Num a, Show a) => Decoder a -> Spec
signedLargeIntegral decoder = do
  it "Large negative" $ do
    decode decoder "-12345678912345678912345678912345678912345678912345678912345"
      `shouldBe` Success "" (-12345678912345678912345678912345678912345678912345678912345)

largeFloat :: (Eq a, Fractional a, Show a) => Decoder a -> Spec
largeFloat decoder = do
  it "Large fractional" $ do
    decode decoder "12345678912345678912345678912345678912345.678912345678912345"
      `shouldBe` Success "" 12345678912345678912345678912345678912345.678912345678912345

  it "Large fractional negative" $ do
    decode decoder "-1234567891234567891234567891.2345678912345678912345678912345"
      `shouldBe` Success "" (-1234567891234567891234567891.2345678912345678912345678912345)



maxRealFloat :: forall a. RealFloat a => a
maxRealFloat = encodeFloat m (e' - e)
  where
    e = floatDigits (0 :: a)
    ~(_, e') = floatRange (0 :: a)
    m = floatRadix (0 :: a) ^ e - 1

minRealFloat :: forall a. RealFloat a => a
minRealFloat = encodeFloat 1 $ fst (floatRange (0 :: a)) - floatDigits (0 :: a)



floatEdges :: forall a. (RealFloat a, Show a) => Decoder a -> Spec
floatEdges decoder = do
  it "Largest representable" $ do
    decode decoder (BSLC.pack $ show (maxRealFloat :: a))
      `shouldBe` Success "" (maxRealFloat :: a)

  it "Overlarge" $ do
    decode decoder "1234.5678e123456789"
      `shouldSatisfy` \f -> case f of
                              Success "" v -> v > 0 && isInfinite v
                              _            -> False

  it "Negative largest representable" $ do
    decode decoder (BSLC.pack $ show (negate maxRealFloat :: a))
      `shouldBe` Success "" (negate maxRealFloat :: a)

  it "Underlarge" $ do
    decode decoder "-1234.5678e123456789"
      `shouldSatisfy` \f -> case f of
                              Success "" v -> v < 0 && isInfinite v
                              _            -> False

  it "Smallest representable" $ do
    decode decoder (BSLC.pack $ show (minRealFloat :: a))
      `shouldBe` Success "" (minRealFloat :: a)

  it "Oversmall" $ do
    decode decoder "1234.5678e-123456789"
      `shouldSatisfy` \f -> case f of
                              Success "" v -> v == 0 && not (isNegativeZero v)
                              _            -> False


  it "Negative smallest representable" $ do
    decode decoder (BSLC.pack $ show (negate minRealFloat :: a))
      `shouldBe` Success "" (negate minRealFloat :: a)

  it "Undersmall" $ do
    decode decoder "-1234.5678e-123456789"
      `shouldSatisfy` \f -> case f of
                              Success "" v -> isNegativeZero v
                              _            -> False



raw :: IsString a => (a -> Expectation) -> Spec
raw f = do
  it "Zero" $
    f "0"

  it "Negative zero" $
    f "-0"

  it "Integer" $
    f "123456789"

  it "Negative integer" $
    f "-123456789"

  it "Fractional" $
    f "123456789.123456789"

  it "Negative fractional" $
    f "-123456789.123456789"

  it "Fractional, under one" $
    f "0.0000000123456789"

  it "Negative fractional, under one" $
    f "-0.0000000123456789"

  it "Integer with exponent" $
    f "123456789e123456789"

  it "Integer with positive exponent" $
    f "123456789e+123456789"

  it "Integer with negative exponent" $
    f "123456789e-123456789"

  it "Negative integer with exponent" $
    f "-123456789e123456789"

  it "Negative integer with positive exponent" $
    f "-123456789e+123456789"

  it "Negative integer with negative exponent" $
    f "-123456789e-123456789"

  it "Fractional with exponent" $
    f "123456789e123456789"

  it "Fractional with positive exponent" $
    f "123456789.123456789e+123456789"

  it "Fractional with negative exponent" $
    f "123456789.123456789e-123456789"

  it "Negative fractional with exponent" $
    f "-123456789.123456789e123456789"

  it "Negative fractional with positive exponent" $
    f "-123456789.123456789e+123456789"

  it "Negative fractional with negative exponent" $
    f "-123456789.123456789e-123456789"

  it "Fractional (sub one) with exponent" $
    f "0.00000000123456789e123456789"

  it "Fractional (sub one) with positive exponent" $
    f "0.00000000123456789e+123456789"

  it "Fractional (sub one) with negative exponent" $
    f "0.00000000123456789e-123456789"

  it "Negative fractional (sub one) with exponent" $
    f "-0.00000000123456789e123456789"

  it "Negative fractional (sub one) with positive exponent" $
    f "-0.00000000123456789e+123456789"

  it "Negative fractional (sub one) with negative exponent" $
    f "-0.00000000123456789e-123456789"




number :: Spec
number = do
  describe "Number decoding" $ do
    describe "Word8" $ do
      integral word8
      unsignedIntegral word8
      integralEdges word8

    describe "Word16" $ do
      integral word16
      unsignedIntegral word16
      integralEdges word16

    describe "Word32" $ do
      integral word32
      unsignedIntegral word32
      integralEdges word32

    describe "Word64" $ do
      integral word64
      unsignedIntegral word64
      integralEdges word64

    describe "Word" $ do
      integral word
      unsignedIntegral word
      integralEdges word

    describe "Natural" $ do
      integral natural
      unsignedIntegral natural
      unsignedLargeIntegral natural

    describe "Int8" $ do
      integral int8
      signedIntegral int8
      integralEdges int8

    describe "Int16" $ do
      integral int16
      signedIntegral int16
      integralEdges int16

    describe "Int32" $ do
      integral int32
      signedIntegral int32
      integralEdges int32

    describe "Int64" $ do
      integral int64
      signedIntegral int64
      integralEdges int64

    describe "Int" $ do
      integral int
      signedIntegral int
      integralEdges int

    describe "Integer" $ do
      integral integer
      signedIntegral integer
      unsignedLargeIntegral integer
      signedLargeIntegral integer

    describe "Float" $ do
      integral float
      signedIntegral float
      floatEdges float

    describe "Double" $ do
      integral double
      signedIntegral double
      floatEdges double

    describe "Scientific" $ do
      integral scientific
      signedIntegral scientific
      unsignedLargeIntegral scientific
      signedLargeIntegral scientific
      largeFloat scientific

    describe "Skip" $ do
      describe "No trail" $
        raw $ \a ->
          decode skipNumber a `shouldBe` Success "" ()

      describe "Trail" $
        raw $ \a ->
          decode skipNumber (a <> "A") `shouldBe` Success "A" ()

    describe "Raw" $ do
      describe "No trail" $
        raw $ \a ->
          decode rawNumber a `shouldBe` Success "" a

      describe "Trail" $
        raw $ \a ->
          decode rawNumber (a <> "A") `shouldBe` Success "A" a
