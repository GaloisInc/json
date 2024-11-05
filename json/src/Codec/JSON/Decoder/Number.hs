{-# LANGUAGE BangPatterns
           , OverloadedStrings
           , RankNTypes
           , UnboxedTuples #-}

module Codec.JSON.Decoder.Number
  ( Codec.JSON.Decoder.Number.word8
  , word16
  , word32
  , word64
  , word

  , Codec.JSON.Decoder.Number.int8
  , int16
  , int32
  , int64
  , int

  , float
  , double

  , bignum
  ) where

import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Number.Internal

import           Data.Int
import           Data.Word
import           GHC.Real (infinity)
import           Parser.Lathe
import           Parser.Lathe.Numeric.Integral
import           Parser.Lathe.Numeric.Fractional
import           Parser.Lathe.Radix



{-# INLINE withNumber #-}
withNumber :: (Path -> Parser (Path, Error) a) -> Decoder a
withNumber f =
  Decoder $ \path bits k ->
    case k of
      N -> f path
      _ -> let !bits' = bits <> NumberBit
           in err (path, Mismatch bits' k)



-- | Decode a JSON number as a 'Word8'.
--
--   Fails if the number cannot fit the target representation.
word8 :: Decoder Word8
word8 = withNumber word8P

-- | Decode a JSON number as a 'Word16'.
--
--   Fails if the number cannot fit the target representation.
word16 :: Decoder Word16
word16 = withNumber word16P

-- | Decode a JSON number as a 'Word32'.
--
--   Fails if the number cannot fit the target representation.
word32 :: Decoder Word32
word32 = withNumber word32P

-- | Decode a JSON number as a 'Word64'.
--
--   Fails if the number cannot fit the target representation.
word64 :: Decoder Word64
word64 = withNumber word64P

-- | Decode a JSON number as a 'Word'.
--
--   Fails if the number cannot fit the target representation.
word :: Decoder Word
word = withNumber wordP


-- | Decode a JSON number as an 'Int8'.
--
--   Fails if the number cannot fit the target representation.
int8 :: Decoder Int8
int8 = withNumber int8P

-- | Decode a JSON number as an 'Int16'.
--
--   Fails if the number cannot fit the target representation.
int16 :: Decoder Int16
int16 = withNumber int16P

-- | Decode a JSON number as an 'Int32'.
--
--   Fails if the number cannot fit the target representation.
int32 :: Decoder Int32
int32 = withNumber int32P

-- | Decode a JSON number as an 'Int64'.
--
--   Fails if the number cannot fit the target representation.
int64 :: Decoder Int64
int64 = withNumber int64P

-- | Decode a JSON number as an 'Int'.
--
--   Fails if the number cannot fit the target representation.
int :: Decoder Int
int = withNumber intP


-- | Decode a JSON number as a 'Float'.
--
--   If the number is too large or too small to represent, returns infinity and zero
--   respectively.
float :: Decoder Float
float = withNumber floatP

-- | Decode a JSON number as a 'Double'.
--
--   If the number is too large or too small to represent, returns infinity and zero
--   respectively.
double :: Decoder Double
double = withNumber doubleP



expectedIntegral, hugeExponent :: String
expectedIntegral = "Expected an integral number"
hugeExponent     = "Exponent does not fit into a platform integer"



bits8, bits16, bits32, bits64, bitsPlatform :: String
bits8        = "8-bit"
bits16       = "16-bit"
bits32       = "32-bit"
bits64       = "64-bit"
bitsPlatform = "platform"

word8P :: Path -> Parser (Path, Error) Word8
word8P = wordP_ bits8 fracWord8Dec fracToWord8

word16P :: Path -> Parser (Path, Error) Word16
word16P = wordP_ bits16 fracWord16Dec fracToWord16

word32P :: Path -> Parser (Path, Error) Word32
word32P = wordP_ bits32 fracWord32Dec fracToWord32

word64P :: Path -> Parser (Path, Error) Word64
word64P = wordP_ bits64 fracWord64Dec fracToWord64

wordP :: Path -> Parser (Path, Error) Word
wordP = wordP_ bitsPlatform fracWordDec fracToWord

{-# INLINE wordP_ #-}
wordP_
  :: (Num w, Num v, Show w)
  => String
  -> (forall e. e -> FracWord w -> Int64 -> Parser e (FracWord w))
  -> (FracWord w -> Integer -> OverUnder v)
  -> Path
  -> Parser (Path, Error) v
wordP_ name fracXDec fracToX = \path -> do
  let overflow = Malformed Benign $ showString "Unsigned " $ showString name
                                                               " integer overflow"

  Whole sign digit <- wholeP path
  case sign of
    Plus -> do
      mayV <- case digit of
                Zero      -> pure Nothing
                NonZero w -> do
                  o <- bytesRead
                  f <- fracXDec (path, overflow) (FracWord (fromIntegral w) 1) 1
                  o' <- bytesRead
                  let !n = o' - o + 1
                  pure $ Just (f, n)

      hasFrac <- fracP path
      mayV' <- if not hasFrac
                 then pure mayV
                 else case mayV of
                        Nothing     -> do
                          o <- bytesRead
                          skipUntilEndOr (/= 0x30)
                          o' <- bytesRead
                          let !n = o - o'

                          v@(FracWord _ d) <- fracXDec (path, overflow) (FracWord 0 0) 0
                          pure $! if d == 0
                                    then Nothing
                                    else Just (v, n)

                        Just (v, n) -> do
                          v' <- fracXDec (path, overflow) v n
                          pure $ Just (v', n)

      case mayV' of
        Nothing     -> do
          skipExponentP path
          pure 0

        Just (v, n) -> do
          eiE <- intExponentP path
          case eiE of
            Left _  -> err (path, Malformed Benign hugeExponent)
            Right e ->
              case fracToX v (fromIntegral e + fromIntegral n) of
                Proper r -> pure r
                Over     -> err (path, overflow)
                Under    -> err (path, Malformed Benign expectedIntegral)

    Minus -> negativeZeroP path digit 0



int8P :: Path -> Parser (Path, Error) Int8
int8P = intP_ bits8 fracInt8Dec fracToInt8

int16P :: Path -> Parser (Path, Error) Int16
int16P = intP_ bits16 fracInt16Dec fracToInt16

int32P :: Path -> Parser (Path, Error) Int32
int32P = intP_ bits32 fracInt32Dec fracToInt32

int64P :: Path -> Parser (Path, Error) Int64
int64P = intP_ bits64 fracInt64Dec fracToInt64

intP :: Path -> Parser (Path, Error) Int
intP = intP_ bitsPlatform fracIntDec fracToInt

{-# INLINE intP_ #-}
intP_
  :: (Num w, Num i)
  => String
  -> (forall e. e -> Sign -> FracInt w -> Int64 -> Parser e (FracInt w))
  -> (Sign -> FracInt w -> Integer -> OverUnder i)
  -> Path
  -> Parser (Path, Error) i
intP_ name fracXDec fracToX = \path -> do
  let overflow = Malformed Benign $ showString "Signed " $ showString name
                                                             " integer overflow"

  Whole sign digit <- wholeP path
  mayI <- case digit of
            Zero      -> pure Nothing
            NonZero w -> do
              o <- bytesRead
              f <- fracXDec (path, overflow) sign (FracInt (fromIntegral w) 1) 1
              o' <- bytesRead
              let !n = o' - o + 1
              pure $ Just (f, n)

  hasFrac <- fracP path
  mayI' <- if not hasFrac
             then pure mayI
             else case mayI of
                    Nothing -> do
                      o <- bytesRead
                      skipUntilEndOr (/= 0x30)
                      o' <- bytesRead
                      let !n = o - o'

                      i@(FracInt _ d) <- fracXDec (path, overflow) sign (FracInt 0 0) 0
                      pure $! if d == 0
                                then Nothing
                                else Just (i, n)

                    Just (i, n) -> do
                      i' <- fracXDec (path, overflow) sign i n
                      pure $ Just (i', n)

  case mayI' of
    Nothing -> do
      skipExponentP path
      pure 0

    Just (i, n) -> do
      eiE <- intExponentP path
      case eiE of
        Left _  -> err (path, Malformed Benign hugeExponent)
        Right e ->
          case fracToX sign i (fromIntegral e + fromIntegral n) of
            Proper r -> pure r
            Over     -> err (path, overflow)
            Under    -> err (path, Malformed Benign expectedIntegral)



floatP :: Path -> Parser (Path, Error) Float
floatP = floatP_ fracFloat23Dec fracToFloat

doubleP :: Path -> Parser (Path, Error) Double
doubleP = floatP_ fracFloat52Dec fracToDouble

{-# INLINE floatP_ #-}
floatP_
  :: (Num w, Fractional f)
  => (forall e. FracFloat w -> Parser e (FracFloat w))
  -> (Sign -> FracFloat w -> Integer -> f)
  -> Path
  -> Parser (Path, Error) f
floatP_ fracXDec fracToX = \path -> do
  Whole sign digit <- wholeP path
  mayF <- case digit of
            Zero      -> pure Nothing
            NonZero w -> do
              o <- bytesRead
              f <- fracXDec $ FracFloat (fromIntegral w) 1
              o' <- bytesRead
              let !n = o' - o + 1
              pure $ Just (f, n)

  hasFrac <- fracP path
  mayF' <- if not hasFrac
             then pure mayF
             else case mayF of
                    Nothing -> do
                      o <- bytesRead
                      skipUntilEndOr (/= 0x30)
                      o' <- bytesRead
                      let !n = o - o'

                      f <- fracXDec (FracFloat 0 0)
                      o'' <- bytesRead
                      pure $! if o'' == o'
                                then Nothing
                                else Just (f, n)

                    Just (f, n) -> do
                      f' <- fracXDec f
                      pure $ Just (f', n)

  case mayF' of
    Nothing -> do
      skipExponentP path
      pure $! case sign of
                Plus  -> 0.0
                Minus -> -0.0

    Just (f, n) -> do
      eiE <- intExponentP path
      case eiE of
        Left signE -> do
          skipUntilEndOr $ maybe True (\_ -> False) . dec
          pure $! case signE of
                    Plus  -> case sign of
                               Plus  -> fromRational infinity
                               Minus -> fromRational (negate infinity)

                    Minus -> case sign of
                               Plus  -> 0.0
                               Minus -> -0.0

        Right e -> pure $! fracToX sign f (fromIntegral e + fromIntegral n)



-- | Decode a JSON number as a two-tuple of decimal significand and decimal exponent.
bignum :: Decoder (Integer, Integer)
bignum = withNumber bignumP

bignumP :: Path -> Parser (Path, Error) (Integer, Integer)
bignumP path = do
  Whole sign digit <- wholeP path

  mayN <- case digit of
            Zero      -> pure Nothing
            NonZero w -> Just <$> wholeNaturalDec (fromIntegral w)

  hasFrac <- fracP path

  mayN' <- if not hasFrac
             then pure $! (\n -> (n, 0)) <$> mayN
             else do
               o <- bytesRead
               first <- case mayN of
                          Nothing -> 0 <$ skipUntilEndOr (/= 0x30)
                          Just n  -> pure (fromIntegral n)

               o' <- bytesRead
               n <- wholeNaturalDec first
               o'' <- bytesRead

               pure $! if o'' == o'
                         then Nothing
                         else let delta = o'' - o
                              in Just (n, delta)

  case mayN' of
    Nothing -> do
      skipExponentP path
      pure (0, 0)

    Just (n, delta)  -> do
      let !intv = case sign of
                    Plus  -> fromIntegral n
                    Minus -> negate (fromIntegral n)

      hasExp <- expP path

      expv <- case hasExp of
                NoExp            -> pure 0
                Exp signE digitE -> do
                  first <- case digitE of
                             Zero      -> 0 <$ skipUntilEndOr (/= 0x30)
                             NonZero w -> pure (fromIntegral w)

                  e <- wholeNaturalDec first
                  pure $! ( case signE of
                              Plus  -> fromIntegral e
                              Minus -> negate (fromIntegral e)
                          )
                            - fromIntegral delta

      pure (intv, expv)
