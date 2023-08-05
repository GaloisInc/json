module Data.Attoparsec.Error
  ( err
  ) where

import           Data.Attoparsec.Internal.Types



{-# INLINE err #-}
err :: String -> Parser i a
err msg = Parser $ \t pos more lose _succ -> lose t pos more [] msg
