module Encoding.Mixed.Error where

import           Data.Word



-- | Invalid character sequences that may be encountered when parsing UTF-8.
data UTF8Error = -- | Byte 1 is @10xxxxxx@
                 Continuation {-# UNPACK #-} !Word8

                 -- | Byte 1 is @11111xxx@
               | Invalid {-# UNPACK #-} !Word8

                 -- | Byte 1 is @1110__1101__@, byte 2 is @10__1__xxxxx@
               | Surrogate {-# UNPACK #-} !Word8

                 -- | Byte 1 is @110__0000__x@
               | Overlong2 {-# UNPACK #-} !Word8

                 -- | Byte 1 is @1110__0000__@, byte 2 is @01__0__xxxxx@
               | Overlong3 {-# UNPACK #-} !Word8

                 -- | Byte 1 is @11110__000__@, byte 2 is @01__00__xxxx@
               | Overlong4 {-# UNPACK #-} !Word8

                 -- | Byte 1 is @110xxxxx@, byte 2 is not @10xxxxx@.
               | Incomplete22 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

                 -- | Byte 1 is @1110xxxx@, byte 2 is not @10xxxxx@.
               | Incomplete23 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

                 -- | Byte 1 is @11110xxx@, byte 2 is not @10xxxxx@.
               | Incomplete24 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

                 -- | Byte 1 is @1110xxxx@, byte 3 is not @10xxxxx@
               | Incomplete33 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                                    {-# UNPACK #-} !Word8

                 -- | Byte 1 is @11110xxx@, byte 3 is not @10xxxxx@
               | Incomplete34 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                                                    {-# UNPACK #-} !Word8

                 -- | Byte 1 is @11110xxx@, byte 4 is not @10xxxxx@.
               | Incomplete4 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
                             {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8

                 -- | Byte 1 is @11110__1__xx@ and either of the @x@es is not 0.
               | Overflow1 {-# UNPACK #-} !Word8

                 -- | Byte 1 is @11110__100__@, byte 2 is @10__00__xxxx@
               | Overflow2 {-# UNPACK #-} !Word8

-- | Default printer for UTF-8 errors.
showsUTF8Error :: UTF8Error -> ShowS
showsUTF8Error err =
  let hexFixed :: Word8 -> ShowS
      hexFixed h =
        let (q, r) = quotRem h 16

            hex n | n < 10    = fromIntegral n + 0x30
                  | otherwise = fromIntegral n + 0x37

        in (:) (toEnum $ hex q) . (:) (toEnum $ hex r)

  in case err of
       Continuation a ->
         showString "Unexpected continuation: " . hexFixed a

       Invalid a ->
         showString "Invalid byte: " . hexFixed a

       Surrogate b ->
         showString "Surrogate character: ED " . hexFixed b

       Overlong2 a ->
         showString "Overlong encoding: " . hexFixed a

       Overlong3 b ->
         showString "Overlong encoding: E0 " . hexFixed b

       Overlong4 b ->
         showString "Overlong encoding: F0 " . hexFixed b

       Incomplete22 a b     ->
         showString "Incomplete character: " . hexFixed a . showChar ' ' . hexFixed b

       Incomplete23 a b     ->
         showString "Incomplete character: " . hexFixed a . showChar ' ' . hexFixed b

       Incomplete24 a b     ->
         showString "Incomplete character: " . hexFixed a . showChar ' ' . hexFixed b

       Incomplete33 a b c   ->
         showString "Incomplete character: " . hexFixed a . showChar ' ' . hexFixed b
                              . showChar ' ' . hexFixed c

       Incomplete34 a b c   ->
         showString "Incomplete character: " . hexFixed a . showChar ' ' . hexFixed b
                              . showChar ' ' . hexFixed c

       Incomplete4 a b c d ->
         showString "Incomplete character: " . hexFixed a . showChar ' ' . hexFixed b
                              . showChar ' ' . hexFixed c . showChar ' ' . hexFixed d

       Overflow1 a ->
         showString "Code point is larger than U+10FFFF: " . hexFixed a

       Overflow2 b ->
         showString "Code point is larger than U+10FFFF: F4 " . hexFixed b



-- | Invalid character sequences that may be encountered when parsing UTF-16.
data UTF16Error = -- | Code unit 1 is @11011__1__xxxxxxxxxx@
                  Invalid1 {-# UNPACK #-} !Word16

                  -- | Code unit 1 is @11011__0__xxxxxxxxxx@,
                  --   code unit 2 is not @11011__1__xxxxxxxxxx@
                | Invalid2 {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16

-- | Default printer for UTF-16 errors.
showsUTF16Error :: UTF16Error -> ShowS
showsUTF16Error err =
  let hexFixed :: Word16 -> ShowS
      hexFixed h0 =
        let (h1, r1) = quotRem h0 16
            (h2, q1) = quotRem h1 16
            (q0, r0) = quotRem h2 16

            hex n | n < 10    = fromIntegral n + 0x30
                  | otherwise = fromIntegral n + 0x37

        in (:) (toEnum $ hex q0) . (:) (toEnum $ hex r0)
         . (:) (toEnum $ hex q1) . (:) (toEnum $ hex r1)

  in case err of
       Invalid1 a ->
         showString "Invalid first code point: " . hexFixed a

       Invalid2 a b ->
         showString "Invalid second code point: " . hexFixed a
                                   . showChar ' ' . hexFixed b



-- | JSON string decoding error.
data DecodingError = UTF8Error UTF8Error
                   | UTF16Error UTF16Error

-- | Default printer for JSON string decoding errors.
showsDecodingError :: DecodingError -> ShowS
showsDecodingError (UTF8Error err) = showString "UTF-8: " . showsUTF8Error err
showsDecodingError (UTF16Error err) = showString "UTF-16: " . showsUTF16Error err



-- | Decoding behavior on encountering an invalid byte sequence.
data DecodingHandling = -- | Fail, returning the provided 'String'.
                        Fail String
                        -- | Continue, inserting a character.
                      | Replace Char
                        -- | Continue, doing nothing.
                      | Ignore



-- | Convenience wrapper.
newtype DecodingHandler = DecodingHandler (DecodingError -> DecodingHandling)
