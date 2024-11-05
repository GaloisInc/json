{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Codec.JSON.Decoder.JSON.Internal
  ( jsonP

  , jsonObjectP
  , jsonArrayP
  , jsonNumberP
  , jsonStringP
  ) where

import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Literals.Internal
import           Codec.JSON.Decoder.Number.Internal
import           Codec.JSON.Decoder.String.Internal
import           Data.JSON.Internal

import           Parser.Lathe
import           Parser.Lathe.Radix



jsonP :: SurrogateHandling_ -> Path -> K -> Parser (Path, Error) ()
jsonP handl path k =
  case k of
    O -> jsonObjectP handl path
    A -> jsonArrayP handl path
    N -> jsonNumberP path
    S -> jsonStringP handl path
    T -> trueP path
    F -> falseP path
    X -> nullP path



jsonObjectP :: SurrogateHandling_ -> Path -> Parser (Path, Error) ()
jsonObjectP handl path = do
  skipEndOr1
  nonempty <- firstKeyP path
  if nonempty
    then go
    else pure ()
  where
    go = do
      (json, _) <- match $ jsonStringP Complain_ path
      nextColonP path
      k <- nextValueP (Key path (JSONKey (JSON json)))
      jsonP handl path k
      more <- nextPairP path
      if more
        then do
          nextKeyP path
          go

        else pure ()



jsonArrayP :: SurrogateHandling_ -> Path -> Parser (Path, Error) ()
jsonArrayP handl path = do
  skipEndOr1
  mayK0 <- firstElementP path
  case mayK0 of
    Nothing -> pure ()
    Just k0 -> go 0 k0
      where
        go !n k = do
          jsonP handl (Index path n) k
          more <- nextElementP path
          if more
            then do
              k' <- nextValueP path
              go (n + 1) k'

            else pure ()



jsonStringP :: SurrogateHandling_ -> Path -> Parser (Path, Error) ()
jsonStringP handl path = do
  skipEndOr1
  stringP_ handl path



jsonNumberP :: Path -> Parser (Path, Error) ()
jsonNumberP path = do
  let digits = skipUntilEndOr $ maybe True (\_ -> False) . dec

  Whole _ whole <- wholeP path
  case whole of
    Zero      -> pure ()
    NonZero _ -> digits

  hasFrac <- fracP path
  if hasFrac
    then digits
    else pure ()

  ex <- expP path
  case ex of
    NoExp   -> pure ()
    Exp _ _ -> digits
