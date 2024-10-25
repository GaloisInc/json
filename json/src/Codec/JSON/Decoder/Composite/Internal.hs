{-# LANGUAGE BangPatterns
           , UnboxedTuples #-}

{-# OPTIONS_HADDOCK hide #-}

module Codec.JSON.Decoder.Composite.Internal
  ( isWhitespace
  , endOfInputP

  , nextValueP
  , determineP

  , firstKeyP
  , nextKeyP
  , nextColonP
  , nextPairP

  , firstElementP
  , nextElementP
  ) where

import           Codec.JSON.Decoder.Internal

import           Data.Word
import           Parser.Lathe
import           Parser.Lathe.Binary (word8)
import           Parser.Lathe.Radix



nextValueP :: Path -> Parser (Path, Error) K
nextValueP path = do
  _ <- skipUntilEndOr (not . isWhitespace)
  determineP path

determineP :: Path -> Parser (Path, Error) K
determineP path =
  ( do w <- word8 $ Left AbruptEnd

       let !(# e #) = case w of
                        0x22 -> (# Right S #)
                        0x2D -> (# Right N #)
                        0x5B -> (# Right A #)
                        0x66 -> (# Right F #)
                        0x6E -> (# Right X #)
                        0x74 -> (# Right T #)
                        0x7B -> (# Right O #)
                        _    ->
                          case dec w of
                            Just _ -> (# Right N #)
                            _      ->
                              let str = "Expected a JSON value"
                              in (# Left (Malformed Fatal str) #)
       err e
   )
    `catch` \eiK ->
      case eiK of
        Right k -> pure k
        Left e  -> err (path, e)



firstKeyP :: Path -> Parser (Path, Error) Bool
firstKeyP path = do
  _ <- skipUntilEndOr (not . isWhitespace)
  ( do w <- word8 $ Just AbruptEnd
       case w of
         0x22 -> err Nothing
         0x7D -> pure False
         _    ->
           let str = "Expected a JSON object pair name or a right curly bracket"
           in err $ Just (Malformed Fatal str)
   )
    `catch` \mayK ->
      case mayK of
        Nothing -> pure True
        Just e  -> err (path, e)



firstElementP :: Path -> Parser (Path, Error) (Maybe K)
firstElementP path = do
  _ <- skipUntilEndOr (not . isWhitespace)
  ( do w <- word8 $ Left AbruptEnd
       case w of
         0x22 -> err $ Right S
         0x2D -> err $ Right N
         0x5B -> err $ Right A
         0x5D -> pure Nothing
         0x66 -> err $ Right F
         0x6E -> err $ Right X
         0x74 -> err $ Right T
         0x7B -> err $ Right O
         _    ->
           case dec w of
             Just _ -> err $ Right N
             _      ->
               let str = "Expected a JSON value or a right square bracket"
               in err $ Left (Malformed Fatal str)
   )
    `catch` \eiK ->
      case eiK of
        Right k -> pure (Just k)
        Left e  -> err (path, e)



-- | Check whether a byte is a JSON whitespace character.
isWhitespace :: Word8 -> Bool
isWhitespace w = w == 0x20 || w == 0x09 || w == 0x0A || w == 0x0D

{-# INLINE endOfInputP #-}
endOfInputP :: e -> Parser e ()
endOfInputP e = do
  _ <- skipUntilEndOr (not . isWhitespace)
  end <- atEnd
  if end
    then pure ()
    else err e



nextKeyP :: Path -> Parser (Path, Error) ()
nextKeyP path = do
  _ <- skipUntilEndOr (not . isWhitespace)
  ( do w <- word8 $ Just (path, AbruptEnd)

       let !(# e #) = case w of
                        0x22 -> (# Nothing #)
                        _    ->
                          let str = "Expected a JSON object pair name"
                          in (# Just (path, Malformed Fatal str) #)
       err e
   )
    `catch` \ei ->
      case ei of
        Nothing -> pure ()
        Just e  -> err e

nextColonP :: Path -> Parser (Path, Error) ()
nextColonP path = do
  _ <- skipUntilEndOr (not . isWhitespace)
  w <- word8 (path, AbruptEnd)
  case w of
    0x3A -> pure ()
    _    ->
      let str = "Expected a colon"
      in err (path, Malformed Fatal str)

nextPairP :: Path -> Parser (Path, Error) Bool
nextPairP path = do
  _ <- skipUntilEndOr (not . isWhitespace)
  w <- word8 (path, AbruptEnd)
  case w of
    0x2C -> pure True
    0x7D -> pure False
    _    ->
      let str = "Expected a comma or a right curly bracket"
      in err (path, Malformed Fatal str)



nextElementP :: Path -> Parser (Path, Error) Bool
nextElementP path = do
  _ <- skipUntilEndOr (not . isWhitespace)
  w <- word8 (path, AbruptEnd)
  case w of
    0x2C -> pure True
    0x5D -> pure False
    _    ->
      let str = "Expected a comma or a right square bracket"
      in err (path, Malformed Fatal str)
