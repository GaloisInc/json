{-# LANGUAGE BangPatterns
           , UnboxedTuples #-}

{-# OPTIONS_HADDOCK hide #-}

module Codec.JSON.Decoder.Literals.Internal
  ( trueP
  , falseP
  , nullP
  ) where

import           Codec.JSON.Decoder.Internal

import qualified Data.ByteString.Unsafe as B
import           Parser.Lathe
import           Parser.Lathe.Unsafe



{-# INLINE trueP #-}
trueP :: Path -> Parser (Path, Error) ()
trueP path = do
  let str = "Not a JSON true"

  unsafeRead 4
    ( \b -> if    B.unsafeIndex b 1 == 0x72
               && B.unsafeIndex b 2 == 0x75
               && B.unsafeIndex b 3 == 0x65
              then (# Yes () #)
              else (# No (path, Malformed Fatal str) #)
    )
    (path, Malformed Fatal str)


{-# INLINE falseP #-}
falseP :: Path -> Parser (Path, Error) ()
falseP path = do
  let str = "Not a JSON false"

  unsafeRead 5
    ( \b -> if    B.unsafeIndex b 1 == 0x61
               && B.unsafeIndex b 2 == 0x6C
               && B.unsafeIndex b 3 == 0x73
               && B.unsafeIndex b 4 == 0x65
              then (# Yes () #)
              else (# No (path, Malformed Fatal str) #)
    )
    (path, Malformed Fatal str)


{-# INLINE nullP #-}
nullP :: Path -> Parser (Path, Error) ()
nullP path = do
  let str = "Not a JSON null"

  unsafeRead 4
    ( \b -> if    B.unsafeIndex b 1 == 0x75
               && B.unsafeIndex b 2 == 0x6C
               && B.unsafeIndex b 3 == 0x6C
              then (# Yes () #)
              else (# No (path, Malformed Fatal str) #)
    )
    (path, Malformed Fatal str)
