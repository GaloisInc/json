module Codec.Web.JSON.Build.String
  ( stringUtf8B
  , stringUtf16B

  , textUtf8B
  , textUtf16B

  , lazyTextUtf8B
  , lazyTextUtf16B
  ) where

import           Data.Bits
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Build
import           Data.ByteString.Builder.Prim
import           Data.Text.Array
import           Data.Text.Encoding
import           Data.Text.Internal (Text (..))
import qualified Data.Text.Lazy as LText
import           Data.Word



{-# INLINE hex #-}
hex :: Word16 -> Word16
hex c | c < 10    = c + 0x30
      | otherwise = c + 0x57

{-# INLINE fixed2 #-}
fixed2 :: FixedPrim (Word8, Word8)
fixed2 = word8 >*< word8

{-# INLINE utfshort #-}
utfshort :: FixedPrim Word16
utfshort = (\c -> ( 0x5C {- \\ -}
                  , ( 0x75 {- 'u'  -}
                    , ( fromIntegral . hex $ unsafeShiftR c 12 .&. 0xF
                      , ( fromIntegral . hex $ unsafeShiftR c  8 .&. 0xF
                        , ( fromIntegral . hex $ unsafeShiftR c  4 .&. 0xF
                          , fromIntegral . hex $              c    .&. 0xF
                          )
                        )
                      )
                    )
                  )
            ) >$< word8 >*< word8 >*< word8 >*< word8 >*< word8 >*< word8



stringUtf8B :: String -> Builder
stringUtf8B (c:rest) =
  if c > '\x1F'
    then case c of
           '"'  -> primFixed fixed2 (0x5C {- \\ -}, 0x22 {- "  -}) <> stringUtf8B rest
           '\\' -> primFixed fixed2 (0x5C {- \\ -}, 0x5C {- \\ -}) <> stringUtf8B rest
           _    -> primBounded charUtf8 c <> stringUtf8B rest

    else primFixed utfshort (fromIntegral $ fromEnum c) <> stringUtf8B rest

stringUtf8B [] = mempty



stringUtf16B :: String -> Builder
stringUtf16B (c:rest) =
  let w = fromEnum c
  in if w <= 0x007E {- ~ -}
       then
         case w of
           0x0022 -> primFixed fixed2 (0x5C {- \\ -}, 0x22 {- "  -}) <> stringUtf16B rest
           0x002F -> primFixed fixed2 (0x5C {- \\ -}, 0x2F {- /  -}) <> stringUtf16B rest
           0x005C -> primFixed fixed2 (0x5C {- \\ -}, 0x5C {- \\ -}) <> stringUtf16B rest
           0x0008 -> primFixed fixed2 (0x5C {- \\ -}, 0x62 {- b  -}) <> stringUtf16B rest
           0x000C -> primFixed fixed2 (0x5C {- \\ -}, 0x66 {- f  -}) <> stringUtf16B rest
           0x000A -> primFixed fixed2 (0x5C {- \\ -}, 0x6E {- n  -}) <> stringUtf16B rest
           0x000D -> primFixed fixed2 (0x5C {- \\ -}, 0x72 {- r  -}) <> stringUtf16B rest
           0x0009 -> primFixed fixed2 (0x5C {- \\ -}, 0x74 {- t  -}) <> stringUtf16B rest
           _      -> if w >= 0x0020 {-   -}
                       then Build.word8 (fromIntegral w) <> stringUtf16B rest
                       else primFixed utfshort (fromIntegral w) <> stringUtf16B rest

       else
         if w < 0x10000
           then primFixed utfshort (fromIntegral w) <> stringUtf16B rest
           else
             let yx = w - 0x10000

                 hi = 0xD800 + unsafeShiftR yx 10 .&. 0x3FF
                 lo = 0xDC00 +              yx    .&. 0x3FF

             in primFixed (utfshort >*< utfshort) (fromIntegral hi, fromIntegral lo)
                  <> stringUtf16B rest

stringUtf16B [] = mempty



textUtf8B :: Text -> Builder
textUtf8B = encodeUtf8BuilderEscaped bound
  where
    part w =
      if w > 0x1F
        then if w == 0x22 {- " -} || w == 0x5C {- \\ -}
               then Right (Left w)
               else Left w

        else Right (Right $ fromIntegral w)

    bound :: BoundedPrim Word8
    bound = part >$< eitherB (liftFixedToBounded word8)
                             (eitherB (liftFixedToBounded $ (,) 0x5C {- \\ -} >$< fixed2)
                                      (liftFixedToBounded utfshort))



textUtf16B :: Text -> Builder
textUtf16B (Text arr off len) = go off
  where
    go :: Int -> Builder
    go i
      | i >= off + len = mempty
      | otherwise      =
          let u0 = unsafeIndex arr i
              u1 = unsafeIndex arr (i + 1)
              u2 = unsafeIndex arr (i + 2)
              u3 = unsafeIndex arr (i + 3)
          in case () of
               () | (u0 .&. 0x80) == 0 ->
                     case u0 of
                       0x0022 -> primFixed fixed2 (0x5C {- \ -}, 0x22 {- " -}) <> go (i + 1)
                       0x002F -> primFixed fixed2 (0x5C {- \ -}, 0x2F {- / -}) <> go (i + 1)
                       0x005C -> primFixed fixed2 (0x5C {- \ -}, 0x5C {- \ -}) <> go (i + 1)
                       0x0008 -> primFixed fixed2 (0x5C {- \ -}, 0x62 {- b -}) <> go (i + 1)
                       0x000C -> primFixed fixed2 (0x5C {- \ -}, 0x66 {- f -}) <> go (i + 1)
                       0x000A -> primFixed fixed2 (0x5C {- \ -}, 0x6E {- n -}) <> go (i + 1)
                       0x000D -> primFixed fixed2 (0x5C {- \ -}, 0x72 {- r -}) <> go (i + 1)
                       0x0009 -> primFixed fixed2 (0x5C {- \ -}, 0x74 {- t -}) <> go (i + 1)
                       _      -> if u0 >= 0x0020 {-   -}
                                   then Build.word8 (fromIntegral u0) <> go (i + 1)
                                   else primFixed utfshort (fromIntegral u0) <> go (i + 1)

                  | (u0 .&. 0x40) == 0  -> primFixed utfshort 0xFFFD <> go (i + 1)

                  | (u0 .&. 0x20) == 0  ->
                      let c = unsafeShiftL (fromIntegral u0 .&. 0x1F) 6
                            +              (fromIntegral u1 .&. 0x3F)

                      in primFixed utfshort c <> go (i + 2)

                  | (u0 .&. 0x10) == 0 ->
                      let c = unsafeShiftL (fromIntegral (u0 .&. 0x0F)) 12
                            + unsafeShiftL (fromIntegral (u1 .&. 0x3F))  6
                            +              (fromIntegral (u2 .&. 0x3F))

                      in primFixed utfshort c <> go (i + 3)

                  | (u0 .&. 0x08) == 0 ->
                      let c = unsafeShiftL (fromIntegral (u0 .&. 0x07)) 18
                            + unsafeShiftL (fromIntegral (u1 .&. 0x3F)) 12
                            + unsafeShiftL (fromIntegral (u2 .&. 0x3F))  6
                            +              (fromIntegral (u3 .&. 0x3F))    :: Int

                          yx = c - 0x10000

                          hi = 0xD800 + unsafeShiftR yx 10 .&. 0x3FF
                          lo = 0xDC00 +              yx    .&. 0x3FF

                      in primFixed (utfshort >*< utfshort) (fromIntegral hi, fromIntegral lo)
                           <> go (i + 4)

                  | otherwise          -> primFixed utfshort 0xFFFD <> go (i + 1)



lazyTextUtf8B :: LText.Text -> Builder
lazyTextUtf8B = LText.foldrChunks (mappend . textUtf8B) mempty

lazyTextUtf16B :: LText.Text -> Builder
lazyTextUtf16B = LText.foldrChunks (mappend . textUtf16B) mempty
