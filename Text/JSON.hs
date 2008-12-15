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

    -- ** Instance helpers
  , makeObj, valFromObj

  ) where

import Text.JSON.Types
import Text.JSON.String

import Data.Char
import Data.List
import Data.Int
import Data.Word
import Data.Either
import Control.Monad(liftM,ap,MonadPlus(..))
import Control.Applicative

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet as I
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.IntMap as IntMap

import qualified Data.Array as Array

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

instance Applicative Result where
  (<*>) = ap
  pure  = return

instance Alternative Result where
  Ok a    <|> _ = Ok a
  Error _ <|> b = b
  empty         = Error "empty"

instance MonadPlus Result where
  Ok a `mplus` _ = Ok a
  _ `mplus` x    = x
  mzero          = Error "Result: MonadPlus.empty"

instance Monad Result where
  return x      = Ok x
  fail x        = Error x
  Ok a >>= f    = f a
  Error x >>= _ = Error x

-- | Convenient error generation
mkError :: String -> Result a
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
  showJSON = encJSString show
  readJSON = decJSString "Ordering" readOrd
    where
     readOrd x = 
       case x of
         "LT" -> return LT
	 "EQ" -> return EQ
	 "GT" -> return GT
	 _    -> mkError ("Unable to read Ordering")

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

-- container types:

instance (Ord a, JSON a, JSON b) => JSON (M.Map a b) where
  showJSON = encJSDict M.toList

   -- backwards compatibility..
  readJSON a@JSArray{}  = M.fromList <$> readJSON a
  readJSON o = decJSDict "Map" M.fromList o
-- alternate (array) mapping:
-- showJSON = showJSArray M.toList
-- readJSON = readJSArray "IntMap" IntMap.fromList

instance (JSON a) => JSON (IntMap.IntMap a) where
  showJSON = encJSDict IntMap.toList
  readJSON = decJSDict "IntMap" IntMap.fromList
-- alternate (array) mapping:
-- showJSON = showJSArray M.toList
-- readJSON = readJSArray "IntMap" IntMap.fromList

instance (Ord a, JSON a) => JSON (Set.Set a) where
  showJSON = encJSArray Set.toList
  readJSON = decJSArray "Set" Set.fromList

instance (Array.Ix i, JSON i, JSON e) => JSON (Array.Array i e) where
  showJSON = encJSArray Array.assocs
  readJSON = decJSArray "Array" arrayFromList

instance JSON I.IntSet where
  showJSON = encJSArray I.toList
  readJSON = decJSArray "IntSet" I.fromList

-- helper functions for array / object serializers:
arrayFromList :: (Array.Ix i) => [(i,e)] -> Array.Array i e
arrayFromList [] = Array.array undefined []
arrayFromList ls@((i,_):xs) = Array.array bnds ls
       where
        bnds = 
	 foldr (\ (ix,_) (mi,ma) ->
	         let
		  mi1 = min ix mi
		  ma1 = max ix ma
		 in
		 mi1 `seq` ma1 `seq` (mi1,ma1))
	       (i,i)
	       xs

-- -----------------------------------------------------------------
-- ByteStrings

instance JSON S.ByteString where
  showJSON = encJSString S.unpack
  readJSON = decJSString "ByteString" (return . S.pack)

instance JSON L.ByteString where
  showJSON = encJSString L.unpack
  readJSON = decJSString "Lazy.ByteString" (return . L.pack)

-- -----------------------------------------------------------------
-- Instance Helpers

makeObj :: [(String, JSValue)] -> JSValue
makeObj = JSObject . toJSObject

-- | Pull a value out of a JSON object.
valFromObj :: JSON a => String -> JSObject JSValue -> Result a
valFromObj k o = maybe (Error $ "valFromObj: Could not find key: " ++ show k)
                       readJSON
		       (lookup k (fromJSObject o))

encJSString :: (a -> String) -> a -> JSValue
encJSString f v = JSString (toJSString (f v))

decJSString :: String -> (String -> Result a) -> JSValue -> Result a
decJSString _ f (JSString s) = f (fromJSString s)
decJSString l _ _ = mkError ("readJSON{"++l++"}: unable to parse string value")

encJSArray :: (JSON a) => (b-> [a]) -> b -> JSValue
encJSArray f v = showJSON (f v)

decJSArray :: (JSON a) => String -> ([a] -> b) -> JSValue -> Result b
decJSArray _ f a@JSArray{} = f <$> readJSON a
decJSArray l _ _ = mkError ("readJSON{"++l++"}: unable to parse array value")

encJSDict :: (JSON a, JSON b) => (c -> [(a,b)]) -> c -> JSValue
encJSDict f v = makeObj $ 
  map (\ (x,y) -> (showJSValue (showJSON x) "", showJSON y)) (f v)

decJSDict :: (JSON a, JSON b) => String -> ([(a,b)] -> c) -> JSValue -> Result c
decJSDict _ f (JSObject o) = mapM rd (fromJSObject o) >>= return . f
   where
     rd (a,b) = do
       pa <- decode a
       pb <- readJSON b
       return (pa,pb)
decJSDict l _ _ = mkError ("readJSON{"++l ++ "}: unable to read dict; expected JSON object")
