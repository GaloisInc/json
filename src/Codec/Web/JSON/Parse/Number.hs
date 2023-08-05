{-# LANGUAGE AllowAmbiguousTypes
           , BangPatterns
           , ScopedTypeVariables
           , TypeApplications #-}

module Codec.Web.JSON.Parse.Number
  ( word8P
  , word16P
  , word32P
  , word64P
  , wordP

  , int8P
  , int16P
  , int32P
  , int64P
  , intP

  , floatP
  , doubleP

  , naturalP
  , integerP
  , scientificP

  , skipNumberP
  , copyNumberP
  ) where

import           Data.Attoparsec.Error
import qualified Data.ByteString.Lazy.Copy as BSL

import           Data.Attoparsec.ByteString
import           Data.Bits
import           Data.Int
import           Data.Scientific hiding (scientificP)
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Real (infinity)
import           Numeric
import           Numeric.Natural



{-# INLINE is0to9 #-}
is0to9 :: Word8 -> Bool
is0to9 w = w <= 0x39 {- 9 -} && w >= 0x30 {- 0 -}

{-# INLINE is0 #-}
is0 :: Word8 -> Bool
is0 = (== 0x30 {- 0 -})

{-# INLINE is1to9 #-}
is1to9 :: Word8 -> Bool
is1to9 w = w <= 0x39 {- 9 -} && w >= 0x31 {- 1 -}

{-# INLINE toDigit #-}
toDigit :: Integral a => Word8 -> a
toDigit w = fromIntegral w - 0x30 {- 0 -}

{-# INLINE isPoint #-}
isPoint :: Word8 -> Bool
isPoint = (== 0x2e {- . -})

{-# INLINE isExponent #-}
isExponent :: Word8 -> Bool
isExponent w = w == 0x45 {- E -} || w == 0x65 {- e -}

{-# INLINE isMinus #-}
isMinus :: Word8 -> Bool
isMinus = (== 0x2d {- - -})

{-# INLINE matchNegative #-}
matchNegative :: Parser Bool
matchNegative = do
  w <- peekWord8'
  if isMinus w
    then True <$ anyWord8
    else pure False



data Exponent a = Overflow
                | Exp a
                | Underflow
                  deriving Show

{-# INLINE exponentP #-}
-- | Parses an exponent together with the optional preceding @+@ or @-@.
--
--   @a@ must be signed.
--
--   WARNING: does not clean up trailing digits if over-/underflowing.
exponentP :: (Bounded a, Integral a) => Parser (Exponent a)
exponentP = do
  negative <- do w <- peekWord8'
                 case w of
                   0x2b {- + -} -> False <$ anyWord8
                   0x2d {- - -} -> True <$ anyWord8
                   _            -> pure False

  let (q0, r0) = quotRem maxBound 10
      q1       = quot    q0       10

      go n
        | n > q1    = pure n
        | otherwise = do
            mayw <- peekWord8
            case mayw of
              Just w | is0to9 w -> do _ <- anyWord8
                                      go $ n * 10 + toDigit w

              _                 -> pure n

  w <- peekWord8'

  if is0 w
    then do _ <- anyWord8
            mayx <- peekWord8
            case mayx of
              Just x | is0to9 x -> err "Leading zero in the exponent"
              _                 -> pure $ Exp 0

    else if is1to9 w
           then do
             _ <- anyWord8
             a <- go (toDigit w)

             mayb <- do mayLast <- peekWord8
                        case mayLast of
                          Just l
                            | is0to9 l -> Just (toDigit l) <$ anyWord8

                          _ -> pure Nothing

             case mayb of
               Just b ->
                 let (msg, v, t) | negative  = (Underflow, negate a * 10 - b, b - 1)
                                 | otherwise = (Overflow , a * 10 + b, b)

                 in if a > q0 || (a == q0 && t > r0)
                      then pure msg

                      else do
                        mayC <- peekWord8
                        case mayC of
                          Just c | is0to9 c -> pure msg
                          _                 -> pure $ Exp v

               Nothing -> pure . Exp $ if negative then negate a else a

           else err $ "Unexpected byte 0x" <> showHex w " after exponent letter"



{-# INLINE preciseUnsignedP #-}
-- | Parses a sized unsigned integer, failing early if precision has been exceeded.
--
--   The first value is the total length of the integer input.
--
--   The second value is the distance from the start of the input to the last
--   meaningful digit, if it has been reached, @-1@ otherwise.
--
--   The third value is an unsigned integer that holds topmost digits.
preciseUnsignedP
  :: forall a. (Bounded a, Integral a)
  => a                                 -- ^ Zero
  -> Parser (Int, Maybe Int, a)
preciseUnsignedP n0
  | n0 > q0  = do
      mayw <- peekWord8
      case mayw of
        Just w
          | is0 w    -> (\i -> (i, Just 1, n0)) <$> countZeroes 0
          | is1to9 w -> err "Integer precision exceeded"

        _ -> pure (0, Nothing, n0)

  | otherwise = do
      (i, n1) <- fast 0 n0
      precise i n1
  where
    (q0, r0) = quotRem maxBound 10
    q1       = quot    q0       10 :: a

    fast i n
      | n > q1    = pure (i, n)
      | otherwise = do
          mayw <- peekWord8
          case mayw of
            Just w
              | is0to9 w -> do _ <- anyWord8
                               fast (i + 1) (n * 10 + toDigit w)

            _      -> pure (i, n)

    precise i n = do
      mayw <- peekWord8
      case mayw of
        Just w
          | is0to9 w -> do
              _ <- anyWord8
              let b = toDigit w
              if n > q0 || (n == q0 && b > r0)
                then err "Integer precision exceeded"
                else do
                  let j0 = i + 1
                  j <- countZeroes j0
                  pure (j, Just j0, n * 10 + b)

        _ -> pure (i, Nothing, n)

    countZeroes i = do
      mayw <- peekWord8
      case mayw of
        Just w
          | is0 w -> do _ <- anyWord8
                        countZeroes $! i + 1

          | is1to9 w -> err "Integer precision exceeded"

        _ -> pure i


{-# INLINE allZeroesP #-}
-- | Parses non-negative @0.(0)@, discards the exponent.
allZeroesP :: String -> Parser ()
allZeroesP msg = do
  w0 <- peekWord8'
  if is1to9 w0
    then err msg
    else if is0 w0
           then do
             _ <- anyWord8
             mayw1 <- peekWord8
             case mayw1 of
               Just w1 | is0to9 w1 -> err "Leading zero"
               _                   -> pure ()

           else err $ "Unexpected byte 0x" <> showHex w0 " after optional minus sign"

  mayw1 <- peekWord8
  case mayw1 of
    Just w1
      | isPoint w1 -> do
          _ <- anyWord8
          w2 <- peekWord8'
          if is0 w2
            then
              let go = do
                    mayz <- peekWord8
                    case mayz of
                      Just z | is0 z    -> anyWord8 >> go
                             | is1to9 z -> err msg

                      _ -> pure ()
              in go

            else err $ if is1to9 w2
                         then msg
                         else "Unexpected byte 0x" <> showHex w2 " after decimal point"

    _ -> pure ()

  mayw2 <- peekWord8
  case mayw2 of
    Just w2
      | isExponent w2 -> do
          _ <- anyWord8
          w <- peekWord8'
          if w == 0x2b {- + -} || w == 0x2d {- - -}
            then () <$ anyWord8
            else pure ()

          skipWhile is0to9

    _ -> pure ()



word8P :: Parser Word8
word8P = unsignedSizedP

word16P :: Parser Word16
word16P = unsignedSizedP

word32P :: Parser Word32
word32P = unsignedSizedP

word64P :: Parser Word64
word64P = unsignedSizedP

wordP :: Parser Word
wordP = unsignedSizedP

{-# INLINE unsignedSizedP #-}
-- | Parses any unsigned sized integer.
--
--   Unlike @parseScientific@ keeps track of the meaningful topmost subset of digits,
--   failing early if precision has been exceeded.
unsignedSizedP :: forall a. (Bounded a, Integral a) => Parser a
unsignedSizedP = do
  neg <- peekWord8'
  if isMinus neg
    then do _ <- anyWord8
            0 <$ allZeroesP "Expected unsigned integer, found minus sign"

    else do
      (n, f0, a) <- do w <- anyWord8
                       if is0 w
                         then do mayx <- peekWord8
                                 case mayx of
                                   Just x | is0to9 x -> err "Leading zero"
                                   _                 -> pure (0, Nothing, 0)

                         else if is1to9 w
                                then preciseUnsignedP @a (toDigit w)
                                else err $ "Unexpected byte 0x"
                                        <> showHex w " after optional minus sign"

      mayb <- do mayw <- peekWord8
                 case mayw of
                   Just w
                     | isPoint w -> do
                         _ <- anyWord8
                         x <- peekWord8'
                         if is0to9 x
                           then Just <$> preciseUnsignedP @a a
                           else err $ "Unexpected byte 0x"
                                   <> showHex x " after decimal point"

                   _ -> pure Nothing

      mayc <- do mayw <- peekWord8
                 case mayw of
                   Just w
                     | isExponent w -> do
                         _ <- anyWord8
                         Just <$> exponentP @Int

                   _ -> pure Nothing

      let bound :: Scientific -> Parser a
          bound sci = case toBoundedInteger sci of
                        Just x  -> pure x
                        Nothing -> err "Number cannot be fit into an integer"

      case mayc of
        Nothing -> bound $
                     case mayb of
                       Just (m, f1, b) -> scientific (fromIntegral b) $
                                            case f0 of
                                              Just f  -> n - f
                                              Nothing -> case f1 of
                                                           Just f  -> (-f)
                                                           Nothing -> (-m)

                       Nothing         -> scientific (fromIntegral a) (maybe 0 (n -) f0)

        Just ec ->
          let isZero = case mayb of
                         Just (_, _, b) -> b == 0 && a == 0
                         Nothing        -> a == 0

          in case ec of
               Exp c_ ->
                 let c = fromIntegral c_
                 in case mayb of
                      Just (m, f1, b) -> bound . scientific (fromIntegral b) $
                                                   case f0 of
                                                     Just f  -> c + n - f
                                                     Nothing -> case f1 of
                                                                  Just g  -> c - g
                                                                  Nothing -> c - m

                      Nothing         -> bound . scientific (fromIntegral a) $
                                                   case f0 of
                                                     Just f  -> c + n - f
                                                     Nothing -> c

               Overflow  | isZero    -> pure 0
                         | otherwise -> err "Exponent is too large"

               Underflow | isZero    -> pure 0
                         | otherwise -> err "Exponent is too small"




{-# INLINE preciseSignedP #-}
-- | Parses a sized signed integer, failing early if precision has been exceeded.
--
--   The first value is the total length of the integer input.
--
--   The second value is the distance from the start of the input to the last
--   meaningful digit, if it has been reached, @-1@ otherwise.
--
--   The third value is a unsigned integer that holds topmost digits.
preciseSignedP
  :: forall a. (Bounded a, Integral a)
  => Bool                              -- ^ Is negative
  -> a                                 -- ^ Zero
  -> Parser (Int, Maybe Int, a)
preciseSignedP negative n0
  | n0 > q0  = do
      mayw <- peekWord8
      case mayw of
        Just w
          | is0 w    -> (\i -> (i, Just 1, n0)) <$> countZeroes 0
          | is1to9 w -> err "Integer precision exceeded"

        _ -> pure (0, Nothing, n0)

  | otherwise = do
      (i, n1) <- fast 0 n0
      precise i n1
  where
    (q0, r0) = quotRem maxBound 10
    q1       = quot    q0       10 :: a

    fast i n
      | n > q1    = pure (i, n)
      | otherwise = do
          mayw <- peekWord8
          case mayw of
            Just w
              | is0to9 w -> do _ <- anyWord8
                               fast (i + 1) (n * 10 + toDigit w)

            _      -> pure (i, n)

    precise i n = do
      mayw <- peekWord8
      case mayw of
        Just w
          | is0to9 w -> do
              _ <- anyWord8
              let b = toDigit w

                  r | negative  = r0 + 1
                    | otherwise = r0

              if n > q0 || (n == q0 && b > r)
                then err "Integer precision exceeded"
                else do
                  let j0 = i + 1
                  j <- countZeroes j0
                  pure (j, Just j0, n * 10 + b)

        _ -> pure (i, Nothing, n)

    countZeroes i = do
      mayw <- peekWord8
      case mayw of
        Just w
          | is0 w -> do _ <- anyWord8
                        countZeroes $! i + 1

          | is1to9 w -> err "Integer precision exceeded"

        _ -> pure i



int8P :: Parser Int8
int8P = signedSizedP @Word8

int16P :: Parser Int16
int16P = signedSizedP @Word16

int32P :: Parser Int32
int32P = signedSizedP @Word32

int64P :: Parser Int64
int64P = signedSizedP @Word64

intP :: Parser Int
intP = signedSizedP @Word

{-# INLINE signedSizedP #-}
-- | Parses any signed sized integer.
--
--   Unlike @parseScientific@ keeps track of the meaningful topmost subset of digits,
--   failing early if precision has been exceeded.
--
--   @x@ is the matching unsigned variant of the integer.
signedSizedP :: forall x a. (Bounded a, Integral a, Bounded x, Integral x) => Parser a
signedSizedP = do
  negative <- matchNegative

  let neg x = if negative then negate x else x

  (n, f0, a) <- do w <- anyWord8
                   if is0 w
                     then do mayx <- peekWord8
                             case mayx of
                               Just x | is0to9 x -> err "Leading zero"
                               _                 -> pure (0, Nothing, 0)

                     else if is1to9 w
                            then preciseSignedP @x negative (toDigit w)
                            else err $ "Unexpected byte 0x"
                                    <> showHex w " after optional minus sign"

  mayb <- do mayw <- peekWord8
             case mayw of
               Just w
                 | isPoint w -> do
                     _ <- anyWord8
                     x <- peekWord8'
                     if is0to9 x
                       then Just <$> preciseSignedP @x negative a
                       else err $ "Unexpected byte 0x" <> showHex x " after decimal point"

               _ -> pure Nothing

  mayc <- do mayw <- peekWord8
             case mayw of
               Just w
                 | isExponent w -> do
                      _ <- anyWord8
                      Just <$> exponentP @Int

               _ -> pure Nothing

  let bound :: Scientific -> Parser a
      bound sci = case toBoundedInteger sci of
                    Just x  -> pure x
                    Nothing -> err "Number cannot be fit into an integer"

  case mayc of
    Nothing -> bound $
                 case mayb of
                   Just (m, f1, b) -> scientific (neg $ fromIntegral b) $
                                        case f0 of
                                          Just f  -> n - f
                                          Nothing -> case f1 of
                                                       Just f  -> (-f)
                                                       Nothing -> (-m)

                   Nothing         -> scientific (neg $ fromIntegral a) (maybe 0 (n -) f0)

    Just ec ->
      let isZero = case mayb of
                     Just (_, _, b) -> b == 0 && a == 0
                     Nothing        -> a == 0

      in case ec of
           Exp c_ ->
             let c = fromIntegral c_
             in case mayb of
                  Just (m, f1, b) -> bound . scientific (neg $ fromIntegral b) $
                                               case f0 of
                                                 Just f  -> c + n - f
                                                 Nothing -> case f1 of
                                                              Just g  -> c - g
                                                              Nothing -> c - m

                  Nothing         -> bound . scientific (neg $ fromIntegral a) $
                                               case f0 of
                                                 Just f  -> c + n - f
                                                 Nothing -> c

           Overflow  | isZero    -> pure 0
                     | otherwise -> err "Exponent is too large"

           Underflow | isZero    -> pure 0
                     | otherwise -> err "Exponent is too small"



{-# INLINE impreciseP #-}
-- | Parses an unsized unsigned integer and discards lowermost digits that cannot be
--   represented.
--
--   The first value is the total length of the integer input.
--
--   The second value is the distance from the start of the input to the last
--   meaningful digit, if it has been reached.
--
--   The third value is an unsigned integer that holds topmost digits
--   such that @result < 10 * largest@.
impreciseP
  :: forall a
   . Integral a
  => a                          -- ^ Largest representable number
  -> a                          -- ^ Zero
  -> Parser (Int, Maybe Int, a)
impreciseP q = precise 0
  where
    precise i n
      | n >= q    = (\x -> (,,) x (Just i) n) <$> countDigits i
      | otherwise = do
          mayw <- peekWord8
          case mayw of
            Just w | is0to9 w -> do _ <- anyWord8
                                    precise (i + 1) (n * 10 + toDigit w)

            _                 -> pure (i, Nothing, n)

    countDigits i = do
      mayw <- peekWord8
      case mayw of
        Just w | is0to9 w -> do _ <- anyWord8
                                countDigits $! i + 1

        _                 -> pure i



floatP :: Parser Float
floatP = realFloatP @Word @Int8

doubleP :: Parser Double
doubleP = realFloatP @Word @Int16

{-# INLINE realFloatP #-}
-- | Parses any sized floating-point value.
--
--   Unlike @parseScientific@ only keeps track of the meaningful topmost subset of digits,
--   disregarding all other ones.
--
--   @x@ is the significand container type, an unsigned integer with the capacity to
--   hold at least @'floatDigits' ('undefined' :: a)@ binary digits.
--
--   @y@ is the exponent container type, a signed integer with the capacity to hold
--   any number in @'floatRange' ('undefined' :: a)@.
realFloatP
  :: forall x y a. (Bits x, Integral x, Bounded y, Integral y, RealFloat a) => Parser a
realFloatP = do
  negative <- matchNegative

  let neg v = if negative then negate v else v

      q :: x
      q = (-1) `unsafeShiftR` (64 - floatDigits (encodeFloat 0 0 :: a) + 1) + 1

  (n, f0, a) <- do w <- anyWord8
                   if w == 0x30 {- 0 -}
                     then do mayx <- peekWord8
                             case mayx of
                               Just x | is0to9 x -> err "Leading zero"
                               _                 -> pure (0, Nothing, 0)

                     else if is1to9 w
                            then impreciseP q (toDigit w)
                            else err $ "Unexpected byte 0x"
                                    <> showHex w " after optional minus sign"

  mayb <- do mayw <- peekWord8
             case mayw of
               Just w
                 | isPoint w -> do
                     _ <- anyWord8
                     x <- peekWord8'
                     if is0to9 x
                       then Just <$> impreciseP q a
                       else err $ "Unexpected byte 0x" <> showHex x " after decimal point"

               _ -> pure Nothing

  mayc <- do mayw <- peekWord8
             case mayw of
               Just w
                 | isExponent w -> do
                     _ <- anyWord8
                     Just <$> exponentP @y

               _ -> pure Nothing

  case mayc of
    Nothing ->
      pure . neg .
               toRealFloat $
                 case mayb of
                   Just (m, f1, b) -> scientific (fromIntegral b) $
                                         case f0 of
                                           Just f  -> n - f
                                           Nothing -> case f1 of
                                                        Just g  -> (-g)
                                                        Nothing -> (-m)

                   Nothing          -> scientific (fromIntegral a) (maybe 0 (n -) f0)

    Just ec ->
      let isZero = case mayb of
                     Just (_, _, b) -> b == 0 && a == 0
                     Nothing         -> a == 0

      in case ec of
           Exp c_ ->      -- Branch reasoning here matches that in 'parseScientific'
             pure . neg $
                      let c = fromIntegral c_
                      in case mayb of
                           Just (m, f1, b) ->
                             if c >= 0
                               then if a == 0 || n + m <= maxBound - c
                                      then toRealFloat $
                                             scientific (fromIntegral b) $
                                               case f0 of
                                                 Just f  -> c - n - f
                                                 Nothing -> case f1 of
                                                              Just g  -> c - g
                                                              Nothing -> c - m

                                      else if b == 0
                                             then 0
                                             else realToFrac infinity

                               else if n + m <= c - minBound
                                      then toRealFloat $
                                             scientific (fromIntegral b) $
                                               case f0 of
                                                 Just f  -> c - n - f
                                                 Nothing -> case f1 of
                                                              Just g  -> c - g
                                                              Nothing -> c - m

                                      else 0

                           Nothing      ->
                             if c <= 0 || n <= maxBound - c
                               then toRealFloat $ scientific (fromIntegral a) c

                               else if a == 0
                                      then 0
                                      else realToFrac infinity

           Overflow  -> do skipWhile is0to9
                           pure . neg $ if isZero
                                          then 0
                                          else realToFrac infinity

           Underflow -> neg 0 <$ skipWhile is0to9




naturalP :: Parser Natural
naturalP = do
  neg <- peekWord8'
  if isMinus neg
    then do _ <- anyWord8
            0 <$ allZeroesP "Expected unsigned integer, found minus sign"

    else do
      sci <- scientificP' False
      case floatingOrInteger sci of
        Right int         -> pure int
        Left (_ :: Float) -> err "Number is fractional"

integerP :: Parser Integer
integerP = do
  negative <- matchNegative
  sci <- scientificP' negative
  case floatingOrInteger sci of
    Right int         -> pure int
    Left (_ :: Float) -> err "Number is fractional"



scientificP :: Parser Scientific
scientificP = do
  negative <- matchNegative
  scientificP' negative



{-# INLINE preciseInfiniteP #-}
-- | Parses any number of digits.
--
--   The first value is the total length of the integer input.
preciseInfiniteP :: Natural -> Parser (Int, Natural)
preciseInfiniteP n0 = go 0 n0 0 0
  where
   q0 = quot maxBound 10 :: Word

   -- Accumulate in a sized containers first so we don't have to multiply
   -- the entire number by 10 constantly.
   go :: Int -> Natural -> Word -> Word -> Parser (Int, Natural)
   go !i n j x = do
     mayw <- peekWord8
     case mayw of
       Just w
         | is0to9 w -> do _ <- anyWord8
                          if x >= q0
                            then let n' = n * (10 ^ j) + fromIntegral x
                                 in n' `seq` go (i + 1) n' 1 (toDigit w)
                            else go (i + 1) n (j + 1) (x * 10 + toDigit w)

       _            -> pure (i, n * (10 ^ j) + fromIntegral x)



{-# INLINE scientificP' #-}
-- | NOTE: Unlike proper floating-point types, 'Scientific' does not support
--         @'infinity'@. As such any number with an exponent too large/small
--         to be represented will return an error.
scientificP' :: Bool -> Parser Scientific
scientificP' negative = do
  let neg v = if negative then negate v else v

  (n, aNat) <- do w <- anyWord8
                  if w == 0x30 {- 0 -}
                    then do mayx <- peekWord8
                            case mayx of
                              Just x | is0to9 x -> err "Leading zero"
                              _                 -> pure (0, 0)

                    else if is1to9 w
                           then preciseInfiniteP (toDigit w)
                           else err $ "Unexpected byte 0x"
                                   <> showHex w " after optional minus sign"

  let a = fromIntegral aNat :: Integer

  mayb <- do mayw <- peekWord8
             case mayw of
               Just w
                 | isPoint w -> do
                     _ <- anyWord8
                     Just . (\ ~(m, b) -> (m, fromIntegral b)) <$> preciseInfiniteP aNat

               _ -> pure Nothing

  mayc <- do mayw <- peekWord8
             case mayw of
               Just w
                 | isExponent w -> do
                     _ <- anyWord8
                     Just <$> exponentP @Int

               _ -> pure Nothing

  case mayc of
    Nothing -> pure . neg $ case mayb of
                              Just (m, b) -> scientific b (-m)
                              Nothing     -> scientific a 0

    Just ec ->
      let tooLarge = "Exponent is too large"
          tooSmall = "Exponent is too small"

          -- Zero is a side case where exponent doesn't matter,
          -- so it's shoved down every failure path.
          isZero = case mayb of
                     Just (_, b) -> b == 0 && a == 0
                     Nothing     -> a == 0

      in case ec of
           Exp c -> case mayb of
                      Just (m, b) ->
                        if c >= 0
                          then -- Can't overflow if integer part is zero
                               if a == 0 || n + m <= maxBound - c
                                 then pure . neg $ scientific b (c - m)
                                 else if a == 0 && b == 0
                                        then pure 0
                                        else err tooLarge

                          -- This assumes worst-case scenario of a denormalized float
                          -- with a significand of 1. Proper checking would require
                          -- traversing @b@ and counting the digits, but this sidecase
                          -- is so impossibly rare there's no need to bother.
                          else if n + m <= c - minBound
                                 then pure . neg $ scientific b (c - m)
                                 else if a == 0 && b == 0
                                        then pure 0
                                        else err tooSmall

                      Nothing      ->
                        -- Integers can't underflow on conversion
                        if c <= 0 || n <= maxBound - c
                          then pure . neg $ scientific a c
                          else if a == 0
                                 then pure 0
                                 else err tooLarge

           Overflow  | isZero    -> do skipWhile is0to9
                                       pure 0
                     | otherwise -> err tooLarge

           Underflow | isZero    -> do skipWhile is0to9
                                       pure 0
                     | otherwise -> err tooSmall



skipNumberP :: Parser ()
skipNumberP = do
  _negative <- do w <- peekWord8'
                  if isMinus w
                    then () <$ anyWord8
                    else pure ()

  w0 <- anyWord8
  if is1to9 w0
    then skipWhile is0to9
    else if is0 w0
           then do
             mayw1 <- peekWord8
             case mayw1 of
               Just w1 | is0to9 w1 -> err "Leading zero"
               _                   -> pure ()

           else err $ "Unexpected byte 0x" <> showHex w0 " after optional minus sign"

  mayw1 <- peekWord8
  case mayw1 of
    Just w1
      | isPoint w1 -> do
          _ <- anyWord8
          w2 <- peekWord8'
          if is0to9 w2
            then skipWhile is0to9
            else err $ "Unexpected byte 0x" <> showHex w2 " after decimal point"

    _ -> pure ()

  mayw2 <- peekWord8
  case mayw2 of
    Just w2
      | isExponent w2 -> do
          _ <- anyWord8
          w <- peekWord8'
          if w == 0x2b {- + -} || w == 0x2d {- - -}
            then () <$ anyWord8
            else pure ()

          skipWhile is0to9

    _ -> pure ()



{-# INLINE pokeByte #-}
pokeByte :: Int -> BSL.Copy -> Word8 -> BSL.Copy
pokeByte len copy w =
  BSL.writeCopy len copy 1 $ \ptr off -> poke (plusPtr ptr off) w



copyNumberP :: Int -> BSL.Copy -> Parser BSL.Copy
copyNumberP chunklen c0 = do
  c1 <- do w <- peekWord8'
           if isMinus w
             then let !c1 = pokeByte chunklen c0 w
                  in c1 <$ anyWord8
             else pure c0

  c2 <- do w0 <- peekWord8'
           if is1to9 w0
             then do _ <- anyWord8
                     skipCopy is0to9 $ pokeByte chunklen c1 w0
             else if is0 w0
                    then do
                      _ <- anyWord8
                      mayw1 <- peekWord8
                      case mayw1 of
                        Just w1 | is0to9 w1 -> err "Leading zero"
                        _                   -> pure $! pokeByte chunklen c1 w0

                    else err $ "Unexpected byte 0x"
                            <> showHex w0 " after optional minus sign"

  c3 <- do mayw1 <- peekWord8
           case mayw1 of
             Just w1
               | isPoint w1 -> do
                   _ <- anyWord8
                   w2 <- peekWord8'
                   if is0to9 w2
                     then do _ <- anyWord8
                             skipCopy is0to9 . flip (pokeByte chunklen) w2
                                             $ pokeByte chunklen c2 w1

                     else err $ "Unexpected byte 0x" <> showHex w2 " after decimal point"

             _ -> pure c2

  mayEx <- peekWord8
  case mayEx of
    Just ex
      | isExponent ex -> do
          _ <- anyWord8
          let c4 = pokeByte chunklen c3 ex

          c5 <- do sign <- peekWord8'
                   if sign == 0x2b {- + -} || sign == 0x2d {- - -}
                     then pokeByte chunklen c4 sign <$ anyWord8
                     else pure c4

          w0 <- peekWord8'
          if is1to9 w0
            then do _ <- anyWord8
                    skipCopy is0to9 $ pokeByte chunklen c5 w0

            else if is0 w0
                   then do
                     _ <- anyWord8

                     mayw1 <- peekWord8
                     case mayw1 of
                       Just w1
                         | is0to9 w1 -> err "Leading zero in the exponent"
                         | otherwise -> do
                             _ <- anyWord8
                             skipCopy is0to9 . flip (pokeByte chunklen) w1
                                             $ pokeByte chunklen c5 w0

                       Nothing -> pure c5

                   else err $ "Unexpected byte 0x" <> showHex w0 " after exponent letter"

    _ -> pure c3
  where
    skipCopy is = go
      where
        go c = do
          mayw <- peekWord8
          case mayw of
            Just w | is w -> let !c' = pokeByte chunklen c w
                             in do _ <- anyWord8
                                   go c'

            _ -> pure c
