{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Codec.JSON.Decoder.Number.Internal
  ( Whole (..)
  , Digit (..)
  , wholeP

  , fracP

  , Exp (..)
  , expP

  , negativeZeroP
  , intExponentP
  , skipExponentP
  ) where

import           Codec.JSON.Decoder.Internal

import           Data.Bits
import           Data.Word
import           Parser.Lathe
import           Parser.Lathe.Binary
import           Parser.Lathe.Numeric.Integral
import           Parser.Lathe.Radix



data Whole = Whole !Sign {-# UNPACK #-} !Digit

data Digit = NonZero {-# UNPACK #-} !Word8
           | Zero

-- | Parses the sign and the first digit of the integral part of the JSON number.
--   Fails if the first digit is a zero followed by another digit.
wholeP :: Path -> Parser (Path, Error) Whole
wholeP path = optionalMinus
  where
    optionalMinus = do
      w0 <- word8 (path, AbruptEnd)
      case w0 of
        0x2D -> do
          let str = "Minus sign must be followed by at least one digit"
          w1 <- word8 (path, Malformed Fatal str)
          int Minus w1

        _    -> int Plus w0


    int sign w =
      case dec w of
        Nothing ->
          let str = "Expected a minus or a digit"
          in err (path, Malformed Fatal str)

        Just i -> do
          if i == 0
            then leadingZeroCheck sign
            else pure $ Whole sign (NonZero i)


    leadingZeroCheck sign =
      ( do w <- word8 True
           err $! maybe True (\_ -> False) (dec w)
      )
        `catch` \good ->
          if good
            then pure $ Whole sign Zero
            else let str = "Leading zeroes are not allowed"
                 in err (path, Malformed Fatal str)



-- | Checks if the number is fractional, consuming the dot character.
fracP :: Path -> Parser (Path, Error) Bool
fracP path = do
  dot <- ( do w <- word8 False
              if w == 0x2E
                then pure True
                else err False
         )
           `catch` pure

  if dot
    then ( do w <- word8 0x00
              err w
         )
           `catch` \w ->
             case dec w of
               Just _  -> pure True
               Nothing ->
                 let str = "Decimal point must be followed by at least one digit"
                 in err (path, Malformed Fatal str)

    else pure False



data Exp = NoExp
         | Exp {-# UNPACK #-} !Sign {-# UNPACK #-} !Digit

-- | Checks if the number has an exponent; if yes, consumes the letter E,
--   optional sign and the first digit.
expP :: Path -> Parser (Path, Error) Exp
expP path = do
  isExp <- ( do w <- word8 False
                if (w .&. 0xDF) == 0x45
                  then pure True
                  else err False
           )
             `catch` pure

  if isExp
    then do
      sign <- ( do w <- word8 Plus
                   case w of
                     0x2B -> pure Plus
                     0x2D -> pure Minus
                     _    -> err Plus
              )
                `catch` pure

      let str = "Exponent letter and optional sign must be followed by at least one digit"

      w <- word8 (path, AbruptEnd)
      case dec w of
        Nothing -> err (path, Malformed Fatal str)
        Just i  -> pure $ Exp sign $ if i == 0
                                       then Zero
                                       else NonZero i

    else pure NoExp



-- | Knowing that the number is negative, cull anything that isn't zero.
negativeZeroP :: Path -> Digit -> z -> Parser (Path, Error) z
negativeZeroP path digit z = do
  let nonNegIntegral = "Expected a non-negative integral number"

  case digit of
    NonZero _ -> err (path, Malformed Benign nonNegIntegral)
    Zero      -> do
      let digits = do
            skipUntilEndOr (/= 0x30)
            ( do w <- word8 True
                 err $! maybe True (\_ -> False) (dec w)
             )
              `catch` \good ->
                if good
                  then pure ()
                  else err (path, Malformed Benign nonNegIntegral)

      hasFrac <- fracP path
      if hasFrac
        then digits
        else pure ()

      e <- expP path
      case e of
        NoExp   -> pure z
        Exp _ _ -> z <$ skipUntilEndOr (maybe True (\_ -> False) . dec)



-- | Parses any exponent that fits into an 'Int'.
intExponentP :: Path -> Parser (Path, Error) (Either Sign Int)
intExponentP path = do
  ex <- expP path
  case ex of
    NoExp          -> pure $ Right 0
    Exp sign digit -> do
      i <- case digit of
             Zero      -> do
               skipUntilEndOr (/= 0x30)
               more <- ( do w <- word8 False
                            err $! maybe False (\_ -> True) $ dec w
                        )
                         `catch` pure
               if more
                 then mapError (\_ -> Right $ Left sign) $ do
                        (i, _) <- wholeIntDec () sign (WholeInt 0) 0
                        pure $ Right i

                 else pure $ Right (WholeInt 0)

             NonZero w -> do
               mapError (\_ -> Right $ Left sign) $ do
                 (i, _) <- wholeIntDec () sign (WholeInt $ fromIntegral w) 1
                 pure $ Right i

      pure $! wholeToInt sign <$> i



skipExponentP :: Path -> Parser (Path, Error) ()
skipExponentP path = do
  ex <- expP path
  case ex of
    NoExp   -> pure ()
    Exp _ _ -> skipUntilEndOr $ maybe True (\_ -> False) . dec
