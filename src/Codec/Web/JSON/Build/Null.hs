module Codec.Web.JSON.Build.Null
  ( nullB
  ) where

import           Data.ByteString.Builder (Builder)
import           Data.ByteString.Builder.Prim



nullB :: Builder
nullB = primMapListFixed word8 [0x6e {- n -}, 0x75 {- u -}, 0x6c {- l -}, 0x6c {- l -}]
