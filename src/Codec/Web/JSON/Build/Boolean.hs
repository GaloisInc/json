module Codec.Web.JSON.Build.Boolean
  ( falseB
  , trueB
  ) where

import           Data.ByteString.Builder (Builder)
import           Data.ByteString.Builder.Prim



falseB :: Builder
falseB = primMapListFixed word8
           [0x66 {- f -}, 0x61 {- a -}, 0x6c {- l -}, 0x73 {- s -}, 0x65 {- e -}]



trueB :: Builder
trueB = primMapListFixed word8 [0x74 {- t -}, 0x72 {- r -}, 0x75 {- u -}, 0x65 {- e -}]
