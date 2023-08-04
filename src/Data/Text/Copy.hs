{-# LANGUAGE RankNTypes #-}

module Data.Text.Copy
  ( Copy (..)
  , emptyCopy
  , writeCopy
  , commitCopy
  ) where

import           Control.Monad.ST
import           Data.Text.Array hiding (empty)
import           Data.Text (empty)
import           Data.Text.Internal (Text (..))



data Copy = Copy {-# UNPACK #-} !Int (forall s. MArray s -> ST s ())

emptyCopy :: Copy
emptyCopy = Copy 0 (\_ -> pure ())

{-# INLINE writeCopy #-}
writeCopy :: Copy -> Int -> (forall s. MArray s -> Int -> ST s ()) -> Copy
writeCopy (Copy off writes) n put = Copy (off + n) (\ptr -> writes ptr <> put ptr off)



{-# INLINE commitCopy #-}
commitCopy :: Copy -> Text
commitCopy (Copy off writes)
  | off <= 0  = empty
  | otherwise = runST $ do
                  marr <- new off
                  writes marr
                  arr <- unsafeFreeze marr
                  pure (Text arr 0 off)
