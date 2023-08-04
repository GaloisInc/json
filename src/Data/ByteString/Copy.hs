module Data.ByteString.Copy
  ( Copy (..)
  , emptyCopy
  , writeCopy
  , commitCopy
  ) where

import           Data.ByteString
import           Data.ByteString.Internal (ByteString (..))
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           System.IO.Unsafe



data Copy = Copy {-# UNPACK #-} !Int (Ptr Word8 -> IO ())

emptyCopy :: Copy
emptyCopy = Copy 0 (\_ -> pure ())

{-# INLINE writeCopy #-}
writeCopy :: Copy -> Int -> (Ptr Word8 -> Int -> IO ()) -> Copy
writeCopy (Copy off pokes) n put = Copy (off + n) (\ptr -> pokes ptr <> put ptr off)



{-# INLINE commitCopy #-}
commitCopy :: Copy -> ByteString
commitCopy (Copy off pokes)
  | off <= 0  = empty
  | otherwise = unsafeDupablePerformIO $ do
                  fptr <- mallocForeignPtrBytes off
                  withForeignPtr fptr pokes
                  pure (BS fptr off)
