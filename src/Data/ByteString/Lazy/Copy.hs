module Data.ByteString.Lazy.Copy
  ( Copy (..)
  , emptyCopy
  , writeCopy
  , forceCopy
  , commitCopy
  ) where

import           Data.ByteString.Internal (ByteString (..))
import qualified Data.ByteString.Lazy as Lazy (empty)
import qualified Data.ByteString.Lazy.Internal as Lazy (ByteString (..))
import           Data.Word
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           System.IO.Unsafe



data Copy =
       Copy {-# UNPACK #-} !Int (Ptr Word8 -> IO ()) (Lazy.ByteString -> Lazy.ByteString)

emptyCopy :: Copy
emptyCopy = Copy 0 (\_ -> pure ()) id

{-# INLINE writeCopy #-}
writeCopy :: Int -> Copy -> Int -> (Ptr Word8 -> Int -> IO ()) -> Copy
writeCopy chunklen copy@(Copy off pokes rest) n put
  | off + n > chunklen = forceCopy copy (Copy n (\ptr -> put ptr 0))
  | otherwise          = Copy (off + n) (\ptr -> pokes ptr <> put ptr off) rest



{-# INLINE forceCopy #-}
forceCopy :: Copy -> ((Lazy.ByteString -> Lazy.ByteString) -> a) -> a
forceCopy (Copy off pokes rest) f
  | off <= 0  = f rest
  | otherwise = f . unsafeDupablePerformIO $ do
                      fptr <- mallocForeignPtrBytes off
                      withForeignPtr fptr pokes
                      pure $ rest . Lazy.Chunk (BS fptr off)

{-# INLINE commitCopy #-}
commitCopy :: Copy -> Lazy.ByteString
commitCopy copy = forceCopy copy ($ Lazy.empty)
