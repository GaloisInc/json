{-# LANGUAGE BangPatterns
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , RankNTypes #-}

module Codec.Web.JSON.Parse.Knot
  ( V (..)
  , vname
  , expectedFound

  , withValueP
  , withObjectP
  , withArrayP
  , withStringP
  , withNumberP
  , withBooleanP
  , withNullP

  , skipValueP
  , skipObjectP
  , skipArrayP
  , skipStringP

  , copyingValueP
  , copyingObjectP
  , copyingArrayP
  , copyingStringP
  , copyingNumberP

  , copyValueP
  , copyObjectP
  , copyArrayP
  ) where

import           Codec.Web.JSON.Parse.Array
import           Codec.Web.JSON.Parse.Boolean
import           Codec.Web.JSON.Parse.Null
import           Codec.Web.JSON.Parse.Number
import           Codec.Web.JSON.Parse.Object
import           Codec.Web.JSON.Parse.String
import           Data.Attoparsec.Error
import           Data.ByteString.Lazy.Copy
import           Encoding.Mixed.Error

import           Data.Attoparsec.ByteString as Atto
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Numeric



-- | First byte of a JSON value.
data V = O -- ^ Object
       | A -- ^ Array
       | S -- ^ String
       | N -- ^ Number
       | F -- ^ False
       | T -- ^ True
       | X -- ^ Null

-- | Full lowercase name for a JSON value.
vname :: V -> String
vname O = "object"
vname A = "array"
vname S = "string"
vname N = "number"
vname F = "false"
vname T = "true"
vname X = "null"

-- | Convenience builder for
--   @Expected \_\_\_\_\_, \_\_\_\_\_ or \_\_\_\_\_, found \_\_\_\_\_@ errors.
expectedFound :: NonEmpty V -> V -> String
expectedFound (x :| []) v =
  showString "Expected " . (vname x <>) . showString ", found " $ vname v

expectedFound (x :| y : zs) v =
  let inter a b cs =
        case cs of
          c:ds -> (vname a <>) . showString ", " . inter b c ds
          []   -> (vname a <>) . showString " or " . (vname b <>)

  in showString "Expected " . inter x y zs . showString ", found " $ vname v



{-# INLINE isWhitespace #-}
isWhitespace :: Word8 -> Bool
isWhitespace w =
  w == 0x20 {-   -} || w == 0x09 {- \t -} || w == 0x0a {- \n -} || w == 0x0d {- \r -}

{-# INLINE isMinus0to9 #-}
isMinus0to9 :: Word8 -> Bool
isMinus0to9 w =
  w <= 0x39 {- 9 -} && (w >= 0x30 {- 0 -} || w == 0x2d {- - -})



{-# INLINE withValueP #-}
withValueP :: (V -> Parser a) -> Parser a
withValueP f = go
  where
    go = do
      w <- peekWord8'
      if isWhitespace w
        then do _ <- anyWord8
                go
        else case w of
               0x7B {- { -}      -> f O
               0x5B {- [ -}      -> f A
               0x22 {- " -}      -> f S
               0x66 {- f -}      -> f F
               0x6e {- n -}      -> f X
               0x74 {- t -}      -> f T
               _ | isMinus0to9 w -> f N
                 | otherwise     ->
                     err . showString "Unexpected structural byte 0x" $ showHex w []


{-# INLINE withObjectP #-}
withObjectP :: Parser a -> Parser a
withObjectP f =
  withValueP $ \v ->
    case v of
      O -> do _ <- anyWord8
              f
      _ -> err $ expectedFound (O :| []) v


{-# INLINE withArrayP #-}
withArrayP :: Parser a -> Parser a
withArrayP f =
  withValueP $ \v ->
    case v of
      A -> do _ <- anyWord8
              f
      _ -> err $ expectedFound (A :| []) v


{-# INLINE withStringP #-}
withStringP :: Parser a -> Parser a
withStringP f =
  withValueP $ \v ->
    case v of
      S -> do _ <- anyWord8
              f
      _ -> err $ expectedFound (S :| []) v


{-# INLINE withNumberP #-}
withNumberP :: Parser a -> Parser a
withNumberP f =
  withValueP $ \v ->
    case v of
      N -> f
      _ -> err $ expectedFound (N :| []) v


{-# INLINE withBooleanP #-}
withBooleanP :: (Bool -> Parser a) -> Parser a
withBooleanP f =
  withValueP $ \v ->
    case v of
      F -> do _ <- anyWord8
              f False
      T -> do _ <- anyWord8
              f True
      _ -> err $ expectedFound (F :| [T]) v


{-# INLINE withNullP #-}
withNullP :: Parser a -> Parser a
withNullP f =
  withValueP $ \v ->
    case v of
      X -> do _ <- anyWord8
              f
      _ -> err $ expectedFound (X :| []) v



skipValueP :: DecodingHandler -> Parser ()
skipValueP handler =
  withValueP $ \v ->
    case v of
      O -> anyWord8 >> skipObjectP handler
      A -> anyWord8 >> skipArrayP handler
      S -> anyWord8 >> skipUtf8P handler
      N -> skipNumberP
      F -> anyWord8 >> falseP
      T -> anyWord8 >> trueP
      X -> anyWord8 >> nullP



skipObjectP :: DecodingHandler -> Parser ()
skipObjectP handler = do
  nonempty <- tilFirstPairP
  if nonempty
    then skipRest
    else pure ()
  where
    skipRest = do
      tilNextNameP $ skipStringP handler
      tilNextColonP
      skipValueP handler
      again <- tilNextPairP
      if again
        then skipRest
        else pure ()



skipArrayP :: DecodingHandler -> Parser ()
skipArrayP handler = do
  nonempty <- tilFirstValueP
  if nonempty
    then skipRest
    else pure ()
  where
    skipRest = do
      skipValueP handler
      again <- tilNextValueP
      if again
        then skipRest
        else pure ()



skipStringP :: DecodingHandler -> Parser ()
skipStringP = withStringP . skipUtf8P



{-# INLINE pokeByte #-}
pokeByte :: Int -> Copy -> Word8 -> Copy
pokeByte len copy w =
  writeCopy len copy 1 $ \ptr off -> poke (plusPtr ptr off) w



{-# INLINE copyingValueP #-}
copyingValueP :: Int -> Copy -> (Copy -> V -> Parser a) -> Parser a
copyingValueP chunklen copy0 f = go copy0
  where
    go copy = do
      w <- peekWord8'
      if isWhitespace w
        then do _ <- anyWord8
                go $ pokeByte chunklen copy w
        else case w of
               0x7B {- { -}      -> f copy O
               0x5B {- [ -}      -> f copy A
               0x22 {- " -}      -> f copy S
               0x66 {- f -}      -> f copy F
               0x6e {- n -}      -> f copy X
               0x74 {- t -}      -> f copy T
               _ | isMinus0to9 w -> f copy N
                 | otherwise     ->
                     err . showString "Unexpected structural byte 0x" $ showHex w []


{-# INLINE copyingObjectP #-}
copyingObjectP :: Int -> Copy -> (Copy -> Parser a) -> Parser a
copyingObjectP chunklen copy0 f =
  copyingValueP chunklen copy0 $ \copy v ->
    case v of
      O -> do w <- anyWord8
              f $ pokeByte chunklen copy w
      _ -> err $ expectedFound (O :| []) v


{-# INLINE copyingArrayP #-}
copyingArrayP :: Int -> Copy -> (Copy -> Parser a) -> Parser a
copyingArrayP chunklen copy0 f =
  copyingValueP chunklen copy0 $ \copy v ->
    case v of
      A -> do w <- anyWord8
              f $ pokeByte chunklen copy w
      _ -> err $ expectedFound (A :| []) v


{-# INLINE copyingStringP #-}
copyingStringP :: Int -> Copy -> (Copy -> Parser a) -> Parser a
copyingStringP chunklen copy0 f =
  copyingValueP chunklen copy0 $ \copy v ->
    case v of
      S -> do w <- anyWord8
              f $ pokeByte chunklen copy w
      _ -> err $ expectedFound (S :| []) v


{-# INLINE copyingNumberP #-}
copyingNumberP :: Int -> Copy -> (Copy -> Parser a) -> Parser a
copyingNumberP chunklen copy0 f =
  copyingValueP chunklen copy0 $ \copy v ->
    case v of
      N -> f copy
      _ -> err $ expectedFound (N :| []) v



copyValueP :: DecodingHandler -> Int -> Copy -> Parser Copy
copyValueP handler chunklen copy0 =
  copyingValueP chunklen copy0 $ \copy v ->
    case v of
      O -> do w <- anyWord8
              copyObjectP handler chunklen (pokeByte chunklen copy w)
      A -> do w <- anyWord8
              copyArrayP handler chunklen (pokeByte chunklen copy w)
      S -> do w <- anyWord8
              copyByteStringUtf8P handler chunklen (pokeByte chunklen copy w)
      N -> copyNumberP chunklen copy
      F -> anyWord8 >> copyFalseP chunklen copy
      T -> anyWord8 >> copyTrueP chunklen copy
      X -> anyWord8 >> copyNullP chunklen copy



copyObjectP :: DecodingHandler -> Int -> Copy -> Parser Copy
copyObjectP handler chunklen c = do
  (nonempty, c') <- copyTilFirstPairP chunklen c
  if nonempty
    then skipRest c'
    else pure c'
  where
    skipRest !c0 = do
      c1 <- copyTilNextNameP chunklen c0
              (withStringP . copyByteStringUtf8P handler chunklen)

      c2 <- copyTilNextColonP chunklen c1
      c3 <- copyValueP handler chunklen c2
      (again, c4) <- copyTilNextPairP chunklen c3
      if again
        then skipRest c4
        else pure c4



copyArrayP :: DecodingHandler -> Int -> Copy -> Parser Copy
copyArrayP handler chunklen c = do
  (nonempty, c') <- copyTilFirstValueP chunklen c
  if nonempty
    then skipRest c'
    else pure c'
  where
    skipRest !c0 = do
      c1 <- copyValueP handler chunklen c0
      (again, c2) <- copyTilNextValueP chunklen c1
      if again
        then skipRest c2
        else pure c2
