{- LANGUAGE BangPatterns, PatternSignatures                         -}
{- LANGUAGE GeneralizedNewtypeDeriving                              -}
{- LANGUAGE FlexibleContexts FlexibleInstances OverlappingInstances -}

--------------------------------------------------------------------
-- |
-- Module    : Text.JSON
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: not portable (FlexibleInstances,NewtypeDeriving,mtl)
--
--------------------------------------------------------------------
--
-- Serialising Haskell values to and from JSON encoded Strings.
--

module Text.JSON (

        -- * The JSType
        JSType(..),

        -- ** Convenient wrapper types
        JSONString(..),
        JSONObject(..),

        -- * JSON serialising
        JSON(..),

        -- ** A writer monad for JSON serialising
        PutJSON(..),
        runPutJSON,     -- :: PutJSON a -> String

        -- ** A state monad with error handling for JSON reading
        GetJSON(..),
        runGetJSON,     -- :: GetJSON a -> String -> Either String a

        -- * Serialising Haskell to JSON format
        encode,     -- :: JSON a => a -> String
        decode,     -- :: JSON a => String -> Either String a

        -- * Low level parsing
        -- ** Reading JSON
        readJSNull, readJSBool, readJSString, readJSInteger, readJSRational,
        readJSArray, readJSObject,

        -- ** Writing JSON
        showJSNull, showJSBool, showJSString, showJSInteger, showJSDouble,
        showJSArray, showJSObject


   ) where

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow (first)

import Data.Char
import Data.List
import Data.Word
import Data.Int
import Data.Ratio
import Data.Generics

import Numeric

-- And needed for the instances:
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.IntSet     as IntSet
-- import qualified Data.Sequence   as Seq
import qualified Data.Map        as M
-- import qualified Data.IntMap     as I
-- import Data.Array.Unboxed

--
-- | JSON values
--
-- The type to which we encode Haskell values. There's a set
-- of primitives, and a couple of heterogenous collection types
-- 
-- Objects:
--
-- An object structure is represented as a pair of curly brackets
-- surrounding zero or more name/value pairs (or members).  A name is a
-- string.  A single colon comes after each name, separating the name
-- from the value.  A single comma separates a value from a
-- following name. 
--
-- Arrays:
--
-- An array structure is represented as square brackets surrounding
-- zero or more values (or elements).  Elements are separated by commas.
--
-- Only valid JSON can be constructed this way
--
data JSType
    = JSNull
    | JSBool     { unJSBool     :: !Bool      }
    | JSInteger  { unJSInteger  :: !Integer   }
    | JSRational { unJSRational :: !Double    }
    | JSArray    { unJSArray    :: [JSType]   }
    | JSString   { unJSString   :: JSONString }
    | JSObject   { unJSObject   :: (JSONObject JSType) }
    deriving (Show, Read, Eq, Ord)

--------------------------------------------------------------------
-- Some simple JSON wrapper types, to avoid overlapping instances

-- | Strings can be represented a little more efficiently in JSON
newtype JSONString   = JSONString { fromJSString :: String        }
    deriving (Eq, Ord, Show, Read)

-- | As can association lists
newtype JSONObject e = JSONObject { fromJSObject :: [(String, e)] }
    deriving (Eq, Ord, Show, Read)

-- -----------------------------------------------------------------
--
-- |Serialising to and from JSON-formatted Strings
--
class JSON a where
    -- | Encode a value into JSON
    showJSON :: a -> PutJSON ()

    -- | Decode a value from JSON
    readJSON :: GetJSON a

-- | The type of JSON writers
newtype PutJSON a = PutJSON { unPut :: Writer ShowS a }
    deriving (Functor, Monad, MonadWriter ShowS)

-- | Run a JSON writer, with some Haskell type, returning a String
runPutJSON :: PutJSON a -> String
runPutJSON = ($ []) . snd . runWriter . unPut

-- | The type of JSON parsers for String
newtype GetJSON a = GetJSON { unGet :: ErrorT String (State String) a }
    deriving (Functor, Monad, MonadState String)

instance Applicative GetJSON where
    pure  = return
    (<*>) = ap

instance Applicative PutJSON where
    pure  = return
    (<*>) = ap

-- | Run a JSON reader on an input String, returning some Haskell value
runGetJSON :: GetJSON a -> String -> Either String a
runGetJSON (GetJSON m) s = evalState (runErrorT m) s

--------------------------------------------------------------------

-- | Encode a value using JSON format to a String
encode :: JSON a => a -> String
encode = runPutJSON . showJSON

-- | Decode a value from a JSON-encoded String
decode :: JSON a => String -> Either String a
decode = runGetJSON readJSON

--------------------------------------------------------------------
--
-- | To ensure we generate valid JSON, we map Haskell types to JSType
-- internally, then pretty print that.
--
instance JSON JSType where

    showJSON x = case x of
      JSBool b     -> tell $ showJSBool b
      JSNull       -> tell $ showJSNull
      JSString  s  -> tell $ showJSString  s
      JSInteger n  -> tell $ showJSInteger n
      JSRational d -> tell $ showJSDouble d
      JSArray    s -> showJSArray s
      JSObject   o -> showJSObject (fromJSObject o)

    readJSON = do
        cs <- get
        case cs of
          'n':_ -> readJSNull
          't':_ -> readJSBool
          'f':_ -> readJSBool
          '"':_ -> readJSString
          '[':_ -> JSArray  <$> readJSArray
          '{':_ -> JSObject . JSONObject <$> readJSObject
          _  -> do
               n <- readJSRational
               return $ if denominator n == 1
                            then JSInteger  (numerator n)
                            else JSRational (fromRational n)

-- -----------------------------------------------------------------
-- Custom instances for wrapper types

instance JSON JSONString where
  showJSON = showJSON <$> JSString
  readJSON = unJSString <$> readJSON

-- -----------------------------------------------------------------
-- Instances
--

instance JSON () where
  showJSON _ = showJSON JSNull
  readJSON   = do JSNull <- readJSON; return ()

instance JSON Bool where
  showJSON = showJSON . JSBool
  readJSON = unJSBool <$> readJSON

instance JSON Char where
  showJSON = showJSON . JSONString . return
  readJSON = head . fromJSString <$> readJSON

-- -----------------------------------------------------------------
-- Integral types

instance JSON Integer where
  showJSON = showJSON . JSInteger
  readJSON = unJSInteger <$> readJSON

-- constrained:
instance JSON Int where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

-- constrained:
instance JSON Word where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

-- -----------------------------------------------------------------

instance JSON Word8 where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

instance JSON Word16 where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

instance JSON Word32 where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

instance JSON Word64 where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

instance JSON Int8 where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

instance JSON Int16 where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

instance JSON Int32 where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

instance JSON Int64 where
  showJSON = showJSON . toInteger
  readJSON = fromIntegral . unJSInteger <$> readJSON

-- -----------------------------------------------------------------

instance JSON Double where
  showJSON = showJSON . JSRational
  readJSON = fromRational <$> readJSRational
    -- can't use JSRational here, due to ambiguous '0' parse
    -- it will parse as Integer.

instance JSON Float where
  showJSON = showJSON . (realToFrac :: Float -> Double)
  readJSON = (realToFrac :: Double -> Float) <$> readJSON

{-
-- Can't serialise rationals very well
--instance JSON Rational where
--  showJSON = showJSDouble . fromRational
--  readJSON = readJSRational
-}

-- -----------------------------------------------------------------
-- sums

-- | Find the name of a constructor
constructor :: Data a => a -> String
constructor = show . toConstr

-- | Return the constructor as an integer string
tag :: (Enum a, Show a) => a -> String
tag = show . fromEnum

-- | Convenience for introducing and destroying constructors
toObject :: JSON a => (String, a) -> PutJSON ()
toObject = showJSON . JSONObject . (:[])

fromObject :: JSON a => GetJSON (String,a)
fromObject = do [a] <- fromJSObject <$> readJSON; return a

-- Ok, Orderings we just write the constructor tag as an Integer, and
-- show the constructor for fun.
instance JSON Ordering where
    showJSON x = toObject (tag x, constructor x)
    readJSON = do (n,_::String) <- fromObject; return (toEnum (read n))

-- We write the tag, then maybe the package.
--
-- > {"0":null}
-- > {"1":"magic"}
--
instance JSON a => JSON (Maybe a) where
  showJSON x = case x of
                    Nothing -> toObject (show (0::Int), ())
                    Just n  -> toObject (show (1::Int), n)

  readJSON = do (n,v) <- fromObject
                return $ case n of
                    "0" -> Nothing
                    "1" -> Just v
                    _ -> fail "Malformed JSON data: invalid tag :: Maybe"

--
-- We have to hide the type of the branch, since they're 
-- different
--
-- Since we can't decompose these things nicely, recursively, the read
-- instance is a bit ugly for my liking.
--
-- We need a way to dispatch without asking for the entire object.
--
instance (JSON a, JSON b) => JSON (Either a b) where
  showJSON x = case x of
                    Left  l -> toObject (show (0::Int), encode l)
                    Right r -> toObject (show (1::Int), encode r)

  readJSON = do (n,v) <- fromObject
                case n of
                    "0" -> let Right x = decode v in return $ Left x
                    "1" -> let Right x = decode v in return $ Right x
                    _   -> fail "Malformed JSON data: invalid tag :: Either"

-- -----------------------------------------------------------------
-- products

-- More hacks to let recursive decent work :(

-- 2 tuples
instance (JSON a, JSON b) => JSON (a,b) where
  showJSON (a,b) = showJSON . JSONObject $ [(show (0 :: Int), encode a)
                                           ,(show (1 :: Int), encode b)]
  readJSON = do [("0",a),("1",b)] <- fromJSObject <$> readJSON
                let Right x = decode a
                    Right y = decode b
                return (x,y)

-- 3 tuples
instance (JSON a, JSON b, JSON c) => JSON (a,b,c) where
  showJSON (a,b,c) = showJSON . JSONObject $ [(show (0::Int), encode a)
                                             ,(show (1::Int), encode b)
                                             ,(show (2::Int), encode c)]

  readJSON = do [("0",a),("1",b),("2",c)] <- fromJSObject <$> readJSON
                let Right x = decode a
                    Right y = decode b
                    Right z = decode c
                return (x,y,z)

-- -----------------------------------------------------------------
-- List-like types

-- We really need to support Strings properly:
instance JSON [Char] where
  showJSON = showJSON . JSONString
  readJSON = fromJSString <$> readJSON

instance JSON a => JSON [a] where
  showJSON = showJSArray
  readJSON = readJSArray

instance JSON S.ByteString where
  showJSON = showJSON . JSONString . S.unpack
  readJSON = S.pack . fromJSString <$> readJSON

instance JSON L.ByteString where
  showJSON = showJSON . JSONString . L.unpack
  readJSON = L.pack . fromJSString <$> readJSON

instance JSON IntSet.IntSet where
  showJSON = showJSON . IntSet.toAscList
  readJSON = IntSet.fromDistinctAscList <$> readJSArray

{-
instance (JSON e) => JSON (Seq.Seq e) where
  showJSON s = showJSON . flip unfoldr s $ \sq -> case Seq.viewl sq of
                    Seq.EmptyL     -> Nothing
                    (Seq.:<) e sq' -> Just (e,sq')
  readJSON = Seq.fromList <$> readJSON
-}

-- ---------------------------------------------------------------------
-- Container types

instance (JSON e) => JSON (JSONObject e) where
  showJSON = showJSObject . fromJSObject
  readJSON = JSONObject <$> readJSObject

instance (Read k, Show k, Ord k, JSON e) => JSON (M.Map k e) where
  showJSON = showJSON . JSONObject . map (first show) . M.toList
  readJSON = M.fromList . map (first read) . fromJSObject <$> readJSON

------------------------------------------------------------------------

{-
instance (JSON i, Num i,Ix i, JSON e) => JSON (Array i e) where

    showJSON = showJSON . elems
    readJSON x = case readJSON x of
                       Just (xs, "") -> Just $ listArray (0,n-1) xs
                            where n = genericLength xs
                       _             -> Nothing
-}

{-
--
-- The IArray UArray e constraint is non portable. Requires flexible instances
--
instance (JSON i, Num i, Ix i, JSON e, IArray UArray e) => JSON (UArray i e) where

    showJSON = showJSON . elems
    readJSON x = case readJSON x of
                       Just (xs, "") -> Just $ listArray (0,n-1) xs
                            where n = genericLength xs
                       _             -> Nothing
-}

-- -----------------------------------------------------------------
-- | Parsing JSON

-- | Find 8 chars context, for error messages
context :: String -> String
context s = take 8 s

-- | Read the JSON null type
readJSNull :: GetJSON JSType
readJSNull = do
  xs <- get
  if "null" `isPrefixOf` xs
        then put (drop 4 xs) >> return JSNull
        else fail $ "Unable to parse JSON null: " ++ context xs

-- | Read the JSON Bool type
readJSBool :: GetJSON JSType
readJSBool = do
  xs <- get
  case () of {_
      | "true"  `isPrefixOf` xs -> put (drop 4 xs) >> return (JSBool True)
      | "false" `isPrefixOf` xs -> put (drop 5 xs) >> return (JSBool False)
      | otherwise               -> fail $ "Unable to parse JSON Bool: " ++ context xs
  }

-- | Read the JSON String type
readJSString :: GetJSON JSType
readJSString = do
  '"' : cs <- get
  parse [] cs

 where parse !rs cs = case cs of
            '\\' : c : ds -> esc rs c ds
            '"'  : ds     -> do put ds
                                return . JSString . JSONString . reverse $ rs
            c    : ds     -> parse (c:rs) ds
            _             -> fail $ "Unable to parse JSON String: unterminated String: "
                                        ++ context cs

       esc rs c cs = case c of
          '\\' -> parse ('\\' : rs) cs
          '"'  -> parse ('"'  : rs) cs
          'n'  -> parse ('\n' : rs) cs
          'r'  -> parse ('\r' : rs) cs
          't'  -> parse ('\t' : rs) cs
          'f'  -> parse ('\f' : rs) cs
          'b'  -> parse ('\b' : rs) cs
          '/'  -> parse ('/'  : rs) cs
          'u'  -> case cs of
                    d1 : d2 : d3 : d4 : cs' ->
                      case readHex [d1,d2,d3,d4] of
                        [(n,"")] -> parse (toEnum n : rs) cs'

                        x -> fail $ "Unable to parse JSON String: invalid hex: " ++ context (show x)
                    _ -> fail $ "Unable to parse JSON String: invalid hex: " ++ context cs
          _ ->  fail $ "Unable to parse JSON String: invalid escape char: " ++ show c

-- | Read an Integer in JSON format
readJSInteger :: GetJSON JSType
readJSInteger = do
  cs <- get
  case cs of
    '-' : ds -> do JSInteger n <- pos ds; return (JSInteger (negate n))
    _        -> pos cs

  where pos ('0':cs)  = put cs >> return (JSInteger 0)
        pos cs        = case span isDigit cs of
                          ([],_)  -> fail $ "Unable to parse JSON Integer: " ++ context cs
                          (xs,ys) -> put ys >> return (JSInteger (read xs))

-- | Read an Integer or Double in JSON format, returning a Rational
readJSRational :: GetJSON Rational
readJSRational = do
  cs <- get
  case cs of
    '-' : ds -> negate <$> pos ds
    _        -> pos cs

  where pos ('0':cs)  = frac 0 cs
        pos cs        = case span isDigit cs of
          ([],_)  -> fail $ "Unable to parse JSON Rational: " ++ context cs
          (xs,ys) -> frac (fromInteger (read xs)) ys

        frac n cs = case cs of
            '.' : ds ->
              case span isDigit ds of
                ([],_) -> put cs >> return n
                (as,bs) -> let x = read as :: Integer
                               y = 10 ^ (fromIntegral (length as) :: Integer)
                           in exponent' (n + (x % y)) bs
            _ -> exponent' n cs

        exponent' n (c:cs)
          | c == 'e' || c == 'E' = (n*) <$> exp_num cs
        exponent' n cs = put cs >> return n

        exp_num          :: String -> GetJSON Rational
        exp_num ('+':cs)  = exp_digs cs
        exp_num ('-':cs)  = recip <$> exp_digs cs
        exp_num cs        = exp_digs cs

        exp_digs :: String -> GetJSON Rational
        exp_digs cs = case readDec cs of
            [(a,ds)] -> put ds >> return (fromIntegral ((10::Integer) ^ (a::Integer)))
            _        -> fail $ "Unable to parse JSON exponential: " ++ context cs

-- | Read a list in JSON format
readJSArray  :: JSON a => GetJSON [a]
readJSArray  = readSequence '[' ']' ','

-- | Read an object in JSON format
readJSObject :: JSON a => GetJSON [(String,a)]
readJSObject = readAssocs '{' '}' ','


-- | Read a sequence of items
readSequence :: JSON a => Char -> Char -> Char -> GetJSON [a]
readSequence start end sep = do
  zs <- get
  case dropWhile isSpace zs of
    c : cs | c == start ->
        case dropWhile isSpace cs of
            d : ds | d == end -> put (dropWhile isSpace ds) >> return []
            ds                -> put ds >> parse []
    _ -> fail $ "Unable to parse JSON sequence: sequence stars with invalid character: " ++ context zs

  where parse !rs = do
          a  <- readJSON
          ds <- get
          case dropWhile isSpace ds of
            e : es | e == sep -> do put (dropWhile isSpace es)
                                    parse (a:rs)
                   | e == end -> do put (dropWhile isSpace es)
                                    return (reverse (a:rs))
            _ -> fail $ "Unable to parse JSON sequence: unterminated sequence: " ++ context ds


-- | Read a sequence of JSON labelled fields
readAssocs :: JSON a => Char -> Char -> Char -> GetJSON [(String,a)]
readAssocs start end sep = do
  zs <- get
  case dropWhile isSpace zs of
    c:cs | c == start -> case dropWhile isSpace cs of
            d:ds | d == end -> put (dropWhile isSpace ds) >> return []
            ds              -> put ds >> parsePairs []
    _ -> fail "Unable to parse JSON object: unterminated sequence"

  where parsePairs !rs = do
          a  <- do (JSString (JSONString k))  <- readJSString
                   ds <- get
                   case dropWhile isSpace ds of
                       ':':es -> do put (dropWhile isSpace es)
                                    v <- readJSON
                                    return (k,v)
                       _      -> fail $ "Malformed JSON labelled field: " ++ context ds

          ds <- get
          case dropWhile isSpace ds of
            e : es | e == sep -> do put (dropWhile isSpace es)
                                    parsePairs (a:rs)
                   | e == end -> do put (dropWhile isSpace es)
                                    return (reverse (a:rs))
            _ -> fail $ "Unable to parse JSON object: unterminated sequence: "
                            ++ context ds


-- -----------------------------------------------------------------
-- | Writing JSON

-- | Write the JSON null type
showJSNull :: ShowS
showJSNull = showString "null"

-- | Write the JSON Bool type
showJSBool :: Bool -> ShowS
showJSBool b = showString (if b then "true" else "false")

-- | Write the JSON String type
showJSString :: JSONString -> ShowS
showJSString (JSONString xs) = quote . foldr (.) quote (map sh xs)
  where
        quote = showChar '"'
        sh c  = case c of
                  '"'  -> showString "\\\""
                  '\\' -> showString "\\\\"
                  '\n' -> showString "\\n"
                  '\r' -> showString "\\r"
                  '\t' -> showString "\\t"
                  '\f' -> showString "\\f"
                  '\b' -> showString "\\b"
                  _ | n < 32 -> showString "\\u"
                       . showHex d1 . showHex d2 . showHex d3 . showHex d4
                  _ -> showChar c
          where n = fromEnum c
                (d1,n1) = n  `divMod` 0x1000
                (d2,n2) = n1 `divMod` 0x0100
                (d3,d4) = n2 `divMod` 0x0010

-- | Write the JSON Integer type
showJSInteger :: Integer -> ShowS
showJSInteger = shows

-- | Show a Double in JSON format
showJSDouble :: Double -> ShowS
showJSDouble x = if isInfinite x || isNaN x then showJSNull else shows x

------------------------------------------------------------------------
-- The rescursive show primitivies run in Put.

-- | Show a list in JSON format
showJSArray :: JSON a => [a] -> PutJSON ()
showJSArray = showSequence '[' ']' ','

-- | Show an association list in JSON format
showJSObject :: JSON a => [(String,a)] -> PutJSON ()
showJSObject = showAssocs '{' '}' ','

-- | Show a generic sequence of pairs in JSON format
showAssocs :: JSON a => Char -> Char -> Char -> [(String,a)] -> PutJSON ()
showAssocs start end sep xs = do
    tell $ showChar start
    sequence_ $ intersperse
                    (tell $ showChar sep)
                           (map (\(s,a) -> do
                                    showJSON (JSONString s)
                                    tell $ showChar ':'
                                    showJSON a
                              ) xs)
    tell $ showChar end

-- | Show a generic sequence in JSON format
showSequence :: JSON a => Char -> Char -> Char -> [a] -> PutJSON ()
showSequence start end sep xs = do
    tell $ showChar start
    sequence_ $ intersperse
                    (tell $ showChar sep)
                           (map showJSON xs)
    tell $ showChar end
