{-# LANGUAGE BangPatterns
           , ExistentialQuantification
           , FlexibleInstances
           , OverloadedStrings
           , TemplateHaskellQuotes #-}

module Codec.JSON.Decoder.Array.Internal
  ( expectsMore
  , expectsFewer
  ) where



expectsMore, expectsFewer :: String
expectsMore  = "Parser expects more array elements"
expectsFewer = "Parser expects fewer array elements"
