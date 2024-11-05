{-# LANGUAGE DerivingStrategies
           , GeneralizedNewtypeDeriving
           , OverloadedStrings #-}

module Codec.JSON.Encoder.Internal
  ( Encoder (..)
  , encode

  , Pair (..)

  , emptyObject
  , object1
  , Object1 (..)
  , pair

  , KeyEncoder (..)

  , emptyArray
  , array1
  , Array1 (..)
  , element

  , string
  , text
  , lazyText

  , Codec.JSON.Encoder.Internal.word8
  , word16
  , word32
  , word64
  , word
  , natural

  , Codec.JSON.Encoder.Internal.int8
  , int16
  , int32
  , int64
  , int
  , integer

  , float
  , double

  , true
  , false
  , Codec.JSON.Encoder.Internal.null

  , json
  ) where

import           Data.JSON.Internal

import           Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Prim as Prim
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text.Encoding as T (encodeUtf8BuilderEscaped)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT (decodeUtf8', encodeUtf8BuilderEscaped)
import           Data.Word
import           Numeric.Natural



-- | The encoder type, wrapping a well-formed raw JSON value.
newtype Encoder = Encoder Builder

instance Show Encoder where
  showsPrec _ enc =
    case LT.decodeUtf8' . toLazyByteString $ encode enc of
      Right txt -> showString $ LT.unpack txt
      Left _    -> showString "<malformed UTF-8>"

-- | Convert any encoded value into a 'Builder'.
encode :: Encoder -> Builder
encode (Encoder ro) = ro



-- | JSON object encoder type.
newtype Pair = Pair (Maybe Object1)



-- | Empty JSON object.
emptyObject :: Encoder
emptyObject = Encoder $ Builder.word16BE 0x7B7D

-- | Encode a non-empty JSON object.
object1 :: Object1 -> Encoder
object1 (Object1 ro) = Encoder $ Builder.word8 0x7B <> ro <> Builder.word8 0x7D

-- | Non-empty JSON object encoder type.
newtype Object1 = Object1 Builder

instance Show Object1 where
  showsPrec _ = showsPrec 0 . object1

instance Semigroup Object1 where
  Object1 a <> Object1 b = Object1 $ a <> Builder.word8 0x2C <> b

-- | Use a JSON object pair name and a JSON value as a non-empty JSON object pair.
pair :: KeyEncoder -> Encoder -> Object1
pair (KeyEncoder (Encoder key)) (Encoder ro) = Object1 $ key <> Builder.word8 0x3A <> ro



-- | Object pair name encoder type.
newtype KeyEncoder = KeyEncoder Encoder
                     deriving newtype Show



-- | Empty JSON array.
emptyArray :: Encoder
emptyArray = Encoder $ Builder.word16BE 0x5B5D

-- | Encode a non-empty JSON array.
array1 :: Array1 -> Encoder
array1 (Array1 ro) = Encoder $ Builder.word8 0x5B <> ro <> Builder.word8 0x5D

-- | Non-empty JSON array encoder type.
newtype Array1 = Array1 Builder

instance Show Array1 where
  showsPrec _ = showsPrec 0 . array1

instance Semigroup Array1 where
  Array1 a <> Array1 b = Array1 $ a <> Builder.word8 0x2C <> b

-- | Use a JSON value as a non-empty JSON array element.
element :: Encoder -> Array1
element (Encoder ro) = Array1 ro



oneBytePrim :: Prim.BoundedPrim Word8
oneBytePrim = jsonPrim_ id (Prim.liftFixedToBounded Prim.word8)

jsonPrim :: Prim.BoundedPrim Char
jsonPrim = jsonPrim_ fromEnum Prim.charUtf8

{-# INLINE jsonPrim_ #-}
jsonPrim_ :: Integral b => (a -> b) -> Prim.BoundedPrim a -> Prim.BoundedPrim a
jsonPrim_ conv one =
  spread Prim.>$< Prim.eitherB one
                    ( Prim.eitherB (Prim.liftFixedToBounded Prim.word16BE)
                                   (Prim.liftFixedToBounded escape)
                    )
  where
    spread _w =
      let w = conv _w
      in if w >= 0x20 && w /= 0x22 {- " -} && w /= 0x5C {- \ -}
           then Left _w
           else case w of
                  0x22 -> Right (Left 0x5C22) {- \" -}
                  0x5C -> Right (Left 0x5C5C) {- \\ -}
                  0x08 -> Right (Left 0x5C62) {- \b -}
                  0x0C -> Right (Left 0x5C66) {- \f -}
                  0x0A -> Right (Left 0x5C6E) {- \n -}
                  0x0D -> Right (Left 0x5C72) {- \r -}
                  0x09 -> Right (Left 0x5C74) {- \t -}
                  _    -> Right (Right (fromIntegral w))

    -- Encoding as \u00XX
    escape =
      (,) 0x5C753030 Prim.>$< Prim.word32BE Prim.>*< Prim.word8HexFixed



-- | Encode a t'String' as a JSON string.
string :: String -> Encoder
string s =
  Encoder $
       Builder.word8 0x22
    <> Prim.primMapListBounded jsonPrim s
    <> Builder.word8 0x22

-- | Encode a strict 'Text' as a JSON string.
text :: Text -> Encoder
text t =
  Encoder $
       Builder.word8 0x22
    <> T.encodeUtf8BuilderEscaped oneBytePrim t
    <> Builder.word8 0x22

-- | Encode a lazy 'LT.Text' as a JSON string.
lazyText :: LT.Text -> Encoder
lazyText t =
  Encoder $
       Builder.word8 0x22
    <> LT.encodeUtf8BuilderEscaped oneBytePrim t
    <> Builder.word8 0x22



-- | Encode a 'Word8' as a JSON number.
word8 :: Word8 -> Encoder
word8 w = Encoder $ word8Dec w

-- | Encode a 'Word16' as a JSON number.
word16 :: Word16 -> Encoder
word16 w = Encoder $ word16Dec w

-- | Encode a 'Word32' as a JSON number.
word32 :: Word32 -> Encoder
word32 w = Encoder $ word32Dec w

-- | Encode a 'Word64' as a JSON number.
word64 :: Word64 -> Encoder
word64 w = Encoder $ word64Dec w

-- | Encode a 'Word' as a JSON number.
word :: Word -> Encoder
word w = Encoder $ wordDec w

-- | Encode a 'Natural' as a JSON number.
natural :: Natural -> Encoder
natural w = Encoder . integerDec $ fromIntegral w



-- | Encode an 'Int8' as a JSON number.
int8 :: Int8 -> Encoder
int8 w = Encoder $ int8Dec w

-- | Encode an 'Int16' as a JSON number.
int16 :: Int16 -> Encoder
int16 w = Encoder $ int16Dec w

-- | Encode an 'Int32' as a JSON number.
int32 :: Int32 -> Encoder
int32 w = Encoder $ int32Dec w

-- | Encode an 'Int64' as a JSON number.
int64 :: Int64 -> Encoder
int64 w = Encoder $ int64Dec w

-- | Encode an 'Int' as a JSON number.
int :: Int -> Encoder
int w = Encoder $ intDec w

-- | Encode an 'Integer' as a JSON number.
integer :: Integer -> Encoder
integer w = Encoder $ integerDec w



nan, inf, ninf :: Builder
nan  = byteString "NaN"
inf  = byteString "Infinity"
ninf = byteString "-Infinity"

-- | Encode a 'Float' as a JSON number.
--
--   @NaN@, \(+\infty\) and \(-\infty\) are instead encoded as
--   @\"NaN\"@, @\"Infinity\"@ and @\"-Infinity\"@ JSON strings respectively.
float :: Float -> Encoder
float f =
  Encoder $
    if isNaN f
      then nan
      else if isInfinite f
             then if f >= 0
                    then inf
                    else ninf
             else floatDec f

-- | Encode a 'Float' as a JSON number.
--
--   @NaN@, \(+\infty\) and \(-\infty\) are instead encoded as
--   @\"NaN\"@, @\"Infinity\"@ and @\"-Infinity\"@ JSON strings respectively.
double :: Double -> Encoder
double d =
  Encoder $
    if isNaN d
      then nan
      else if isInfinite d
             then if d >= 0
                    then inf
                    else ninf
             else doubleDec d



true :: Encoder
true = Encoder $ word32BE 0x74727565

false :: Encoder
false = Encoder $
          Prim.primFixed
            ((\_ -> (0x66616C73, 0x65)) Prim.>$< Prim.word32BE Prim.>*< Prim.word8)
            ()



-- | JSON null.
null :: Encoder
null = Encoder $ word32BE 0x6E756C6C



-- | Use a raw value as an encoder.
json :: JSON -> Encoder
json (JSON ro) = Encoder $ lazyByteString ro
