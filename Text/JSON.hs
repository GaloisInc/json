--------------------------------------------------------------------
-- |
-- Module    : Text.JSON
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: portable
--
--------------------------------------------------------------------
--
-- Serialising Haskell values to and from JSON values.
--

module Text.JSON (
    -- * JSON Types
    JSValue(..)

    -- * Serialization to and from JSValues
  , JSON(..)

    -- * Encoding and Decoding
  , Result(..)
  , encode -- :: JSON a => a -> String
  , decode -- :: JSON a => String -> Either String a
  , encodeStrict -- :: JSON a => a -> String
  , decodeStrict -- :: JSON a => String -> Either String a

    -- * Wrapper Types
  , JSString
  , toJSString
  , fromJSString

  , JSObject
  , toJSObject
  , fromJSObject
  , resultToEither

    -- * Serialization to and from Strings.
    -- ** Reading JSON
  , readJSNull, readJSBool, readJSString, readJSRational
  , readJSArray, readJSObject, readJSValue

    -- ** Writing JSON
  , showJSNull, showJSBool, showJSRational, showJSArray
  , showJSObject, showJSValue


  ) where

import Text.JSON.Types
import Text.JSON.String

import Data.Char
import Data.List
import Data.Int
import Data.Word
import Data.Either
import Control.Monad(liftM,ap)

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as I
import qualified Data.Map as M

------------------------------------------------------------------------

-- | Decode a String representing a JSON value 
-- (either an object, array, bool, number, null)
--
-- This is a superset of JSON, as types other than
-- Array and Object are allowed at the top level.
--
decode :: (JSON a) => String -> Result a
decode s = case runGetJSON readJSValue s of
             Right a  -> readJSON a
             Left err -> Error err

-- | Encode a Haskell value into a string, in JSON format.
--
-- This is a superset of JSON, as types other than
-- Array and Object are allowed at the top level.
--
encode :: (JSON a) => a -> String
encode = (flip showJSValue [] . showJSON)

------------------------------------------------------------------------

-- | Decode a String representing a strict JSON value.
-- This follows the spec, and requires top level
-- JSON types to be an Array or Object.
decodeStrict :: (JSON a) => String -> Result a
decodeStrict s = case runGetJSON readJSTopType s of
     Right a  -> readJSON a
     Left err -> Error err

-- | Encode a value as a String in strict JSON format.
-- This follows the spec, and requires all values
-- at the top level to be wrapped in either an Array or Object.
-- JSON types to be an Array or Object.
encodeStrict :: (JSON a) => a -> String
encodeStrict = (flip showJSTopType [] . showJSON)

------------------------------------------------------------------------

-- | The class of types serialisable to and from JSON
class JSON a where
  readJSON  :: JSValue -> Result a
  showJSON  :: a -> JSValue

  readJSONs :: JSValue -> Result [a]
  readJSONs (JSArray as) = mapM readJSON as
  readJSONs _            = mkError "Unable to read list"

  showJSONs :: [a] -> JSValue
  showJSONs = JSArray . map showJSON

-- | A type for parser results
data Result a = Ok a | Error String
  deriving (Eq,Show)

-- | Map Results to Eithers
resultToEither :: Result a -> Either String a
resultToEither (Ok a)    = Right a
resultToEither (Error s) = Left  s

instance Functor Result where fmap = liftM
instance Monad Result where
  return x      = Ok x
  fail x        = Error x
  Ok a >>= f    = f a
  Error x >>= _ = Error x

-- | Convenient error generation
mkError :: (JSON a) => String -> Result a
mkError s = Error s

--------------------------------------------------------------------
--
-- | To ensure we generate valid JSON, we map Haskell types to JSValue
-- internally, then pretty print that.
--
instance JSON JSValue where
    showJSON = id
    readJSON = return

second :: (a -> b) -> (x,a) -> (x,b)
second f (a,b) = (a, f b)

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

--------------------------------------------------------------------
-- Some simple JSON wrapper types, to avoid overlapping instances

instance JSON JSString where
  readJSON (JSString s) = return s
  readJSON _            = mkError "Unable to read JSString"
  showJSON = JSString

instance (JSON a) => JSON (JSObject a) where
  readJSON (JSObject o) =
      let f (x,y) = do y' <- readJSON y; return (x,y')
      in toJSObject `fmap` mapM f (fromJSObject o)
  readJSON _ = mkError "Unable to read JSObject"
  showJSON = JSObject . toJSObject . map (second showJSON) . fromJSObject


-- -----------------------------------------------------------------
-- Instances
--

instance JSON Bool where
  showJSON = JSBool
  readJSON (JSBool b) = return b
  readJSON _          = mkError "Unable to read Bool"

instance JSON Char where
  showJSON  = JSString . toJSString . (:[])
  showJSONs = JSString . toJSString

  readJSON (JSString s) = case fromJSString s of
                            [c] -> return c
                            _ -> mkError "Unable to read Char"
  readJSON _            = mkError "Unable to read Char"

  readJSONs (JSString s)  = return (fromJSString s)
  readJSONs (JSArray a)   = mapM readJSON a
  readJSONs _             = mkError "Unable to read String"

instance JSON Ordering where
  showJSON LT = JSRational (-1)
  showJSON EQ = JSRational 0
  showJSON GT = JSRational 1
  readJSON (JSRational (-1)) = return LT
  readJSON (JSRational 0) = return EQ
  readJSON (JSRational 1) = return GT
  readJSON _ = mkError "Unable to read Ordering"

-- -----------------------------------------------------------------
-- Integral types

instance JSON Integer where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ round i
  readJSON _             = mkError "Unable to read Integer"

-- constrained:
instance JSON Int where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ round i
  readJSON _              = mkError "Unable to read Int"

-- constrained:
instance JSON Word where
  showJSON = JSRational . toRational
  readJSON (JSRational i) = return $ truncate i
  readJSON _             = mkError "Unable to read Word"

-- -----------------------------------------------------------------

instance JSON Word8 where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ truncate i
  readJSON _             = mkError "Unable to read Word8"

instance JSON Word16 where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ truncate i
  readJSON _             = mkError "Unable to read Word16"

instance JSON Word32 where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ truncate i
  readJSON _             = mkError "Unable to read Word32"

instance JSON Word64 where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ truncate i
  readJSON _             = mkError "Unable to read Word64"

instance JSON Int8 where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ truncate i
  readJSON _             = mkError "Unable to read Int8"

instance JSON Int16 where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ truncate i
  readJSON _             = mkError "Unable to read Int16"

instance JSON Int32 where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ truncate i
  readJSON _             = mkError "Unable to read Int32"

instance JSON Int64 where
  showJSON = JSRational . fromIntegral
  readJSON (JSRational i) = return $ truncate i
  readJSON _             = mkError "Unable to read Int64"

-- -----------------------------------------------------------------

instance JSON Double where
  showJSON = JSRational . toRational
  readJSON (JSRational r) = return $ fromRational r
  readJSON _              = mkError "Unable to read Double"
    -- can't use JSRational here, due to ambiguous '0' parse
    -- it will parse as Integer.

instance JSON Float where
  showJSON = JSRational . toRational
  readJSON (JSRational r) = return $ fromRational r
  readJSON _              = mkError "Unable to read Float"

-- -----------------------------------------------------------------
-- Sums

instance (JSON a) => JSON (Maybe a) where
  readJSON (JSObject o) = case "just" `lookup` as of
      Just x -> Just <$> readJSON x
      _      -> case "nothing" `lookup` as of
          Just JSNull -> return Nothing
          _           -> mkError "Unable to read Maybe"
    where as = fromJSObject o
  readJSON _ = mkError "Unable to read Maybe"
  showJSON (Just x) = JSObject $ toJSObject [("just", showJSON x)]
  showJSON Nothing  = JSObject $ toJSObject [("nothing", JSNull)]

instance (JSON a, JSON b) => JSON (Either a b) where
  readJSON (JSObject o) = case "left" `lookup` as of
      Just a  -> Left <$> readJSON a
      Nothing -> case "right" `lookup` as of
          Just b  -> Right <$> readJSON b
          Nothing -> mkError "Unable to read Either"
    where as = fromJSObject o
  readJSON _ = mkError "Unable to read Either"
  showJSON (Left a)  = JSObject $ toJSObject [("left",  showJSON a)]
  showJSON (Right b) = JSObject $ toJSObject [("right", showJSON b)]

-- -----------------------------------------------------------------
-- Products

instance JSON () where
  showJSON _ = JSArray []
  readJSON (JSArray []) = return ()
  readJSON _      = mkError "Unable to read ()"

instance (JSON a, JSON b) => JSON (a,b) where
  showJSON (a,b) = JSArray [ showJSON a, showJSON b ]
  readJSON (JSArray [a,b]) = (,) `fmap` readJSON a `ap` readJSON b
  readJSON _ = mkError "Unable to read Pair"

instance (JSON a, JSON b, JSON c) => JSON (a,b,c) where
  showJSON (a,b,c) = JSArray [ showJSON a, showJSON b, showJSON c ]
  readJSON (JSArray [a,b,c]) = (,,) `fmap`
                                  readJSON a `ap`
                                  readJSON b `ap`
                                  readJSON c
  readJSON _ = mkError "Unable to read Triple"

instance (JSON a, JSON b, JSON c, JSON d) => JSON (a,b,c,d) where
  showJSON (a,b,c,d) = JSArray [showJSON a, showJSON b, showJSON c, showJSON d]
  readJSON (JSArray [a,b,c,d]) = (,,,) `fmap`
                                  readJSON a `ap`
                                  readJSON b `ap`
                                  readJSON c `ap`
                                  readJSON d

  readJSON _ = mkError "Unable to read 4 tuple"

-- -----------------------------------------------------------------
-- List-like types


instance JSON a => JSON [a] where
  showJSON = showJSONs
  readJSON = readJSONs

instance (Ord a, JSON a, JSON b) => JSON (M.Map a b) where
  showJSON = showJSON . M.toList
  readJSON a@(JSArray _) = M.fromList <$> readJSON a
  readJSON _ = mkError "Unable to read Map"

instance JSON I.IntSet where
  showJSON = showJSON . I.toList
  readJSON a@(JSArray _) = I.fromList <$> readJSON a
  readJSON _ = mkError "Unable to read IntSet"

-- -----------------------------------------------------------------
-- ByteStrings

instance JSON S.ByteString where
  showJSON = JSString . toJSString . S.unpack
  readJSON (JSString s) = return $ S.pack $ fromJSString s
  readJSON _ = mkError "Unable to read ByteString"

instance JSON L.ByteString where
  showJSON = JSString . toJSString . L.unpack
  readJSON (JSString s) = return $ L.pack $ fromJSString s
  readJSON _ = mkError "Unable to read ByteString"
