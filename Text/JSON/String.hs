--------------------------------------------------------------------
-- |
-- Module    : Text.JSON.String
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: portable
--
--------------------------------------------------------------------
--
-- Basic support for working with JSON values.
--

module Text.JSON.String (
    -- * Parsing
    --
     GetJSON, runGetJSON

    -- ** Reading JSON
  , readJSNull, readJSBool, readJSString, readJSRational
  , readJSArray, readJSObject

  , readJSValue, readJSTopType

    -- ** Writing JSON
  , showJSNull, showJSBool, showJSRational, showJSArray
  , showJSObject

  , showJSValue, showJSTopType

  ) where

import Text.JSON.Types (JSValue(..),
                        JSString, toJSString, fromJSString,
                        JSObject, toJSObject, fromJSObject)

import Control.Monad (liftM)
import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf)
import Data.Ratio (numerator, denominator, (%))
import Numeric (readHex, readDec, showHex)


-- -----------------------------------------------------------------
-- | Parsing JSON

-- | The type of JSON parsers for String
newtype GetJSON a = GetJSON { un :: String -> Either String (a,String) }

instance Functor GetJSON where fmap = liftM
instance Monad GetJSON where
  return x        = GetJSON (\s -> Right (x,s))
  fail x          = GetJSON (\_ -> Left x)
  GetJSON m >>= f = GetJSON (\s -> case m s of
                                     Left err -> Left err
                                     Right (a,s1) -> un (f a) s1)

-- | Run a JSON reader on an input String, returning some Haskell value.
-- All input will be consumed.
runGetJSON :: GetJSON a -> String -> Either String a
runGetJSON (GetJSON m) s = case m s of
     Left err    -> Left err
     Right (a,t) -> case t of
                        [] -> Right a
                        _  -> Left $ "Invalid tokens at end of JSON string: "++ show (take 10 t)

getInput   :: GetJSON String
getInput    = GetJSON (\s -> Right (s,s))

setInput   :: String -> GetJSON ()
setInput s  = GetJSON (\_ -> Right ((),s))

(<$>) :: Functor f => (a -> b) -> f a -> f b
x <$> y = fmap x y

-------------------------------------------------------------------------

-- | Find 8 chars context, for error messages
context :: String -> String
context s = take 8 s

-- | Read the JSON null type
readJSNull :: GetJSON JSValue
readJSNull = do
  xs <- getInput
  if "null" `isPrefixOf` xs
        then setInput (drop 4 xs) >> return JSNull
        else fail $ "Unable to parse JSON null: " ++ context xs

-- | Read the JSON Bool type
readJSBool :: GetJSON JSValue
readJSBool = do
  xs <- getInput
  case () of {_
      | "true"  `isPrefixOf` xs -> setInput (drop 4 xs) >> return (JSBool True)
      | "false" `isPrefixOf` xs -> setInput (drop 5 xs) >> return (JSBool False)
      | otherwise               -> fail $ "Unable to parse JSON Bool: " ++ context xs
  }

-- | Read the JSON String type
readJSString :: GetJSON JSValue
readJSString = do
  x <- getInput
  case x of
       '"' : cs -> parse [] cs
       _        -> fail $ "Malformed JSON: expecting string: " ++ context x

 where parse rs cs = rs `seq` case cs of
            '\\' : c : ds -> esc rs c ds
            '"'  : ds     -> do setInput ds
                                return . JSString . toJSString . reverse $ rs
            c    : ds
                |  (c `elem` (['\x20'..'\x21'] ++ ['\x23'..'\x5B'])) || (c >='\x5D' && c <= '\x10FFFF')
                          -> parse (c:rs) ds
                | otherwise
                          -> fail $ "Illegal unescaped character in string: " ++ context cs
            _             -> fail $ "Unable to parse JSON String: unterminated String: " ++ context cs

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


-- | Read an Integer or Double in JSON format, returning a Rational
readJSRational :: GetJSON Rational
readJSRational = do
  cs <- getInput
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
                ([],_) -> setInput cs >> return n
                (as,bs) -> let x = read as :: Integer
                               y = 10 ^ (fromIntegral (length as) :: Integer)
                           in exponent' (n + (x % y)) bs
            _ -> exponent' n cs

        exponent' n (c:cs)
          | c == 'e' || c == 'E' = (n*) <$> exp_num cs
        exponent' n cs = setInput cs >> return n

        exp_num          :: String -> GetJSON Rational
        exp_num ('+':cs)  = exp_digs cs
        exp_num ('-':cs)  = recip <$> exp_digs cs
        exp_num cs        = exp_digs cs

        exp_digs :: String -> GetJSON Rational
        exp_digs cs = case readDec cs of
            [(a,ds)] -> do setInput ds
                           return (fromIntegral ((10::Integer) ^ (a::Integer)))
            _        -> fail $ "Unable to parse JSON exponential: " ++ context cs

-- | Read a list in JSON format
readJSArray  :: GetJSON JSValue
readJSArray  = readSequence '[' ']' ',' >>= return . JSArray

-- | Read an object in JSON format
readJSObject :: GetJSON JSValue
readJSObject = readAssocs '{' '}' ',' >>= return . JSObject . toJSObject


-- | Read a sequence of items
readSequence :: Char -> Char -> Char -> GetJSON [JSValue]
readSequence start end sep = do
  zs <- getInput
  case dropWhile isSpace zs of
    c : cs | c == start ->
        case dropWhile isSpace cs of
            d : ds | d == end -> setInput (dropWhile isSpace ds) >> return []
            ds                -> setInput ds >> parse []
    _ -> fail $ "Unable to parse JSON sequence: sequence stars with invalid character: " ++ context zs

  where parse rs = rs `seq` do
          a  <- readJSValue
          ds <- getInput
          case dropWhile isSpace ds of
            e : es | e == sep -> do setInput (dropWhile isSpace es)
                                    parse (a:rs)
                   | e == end -> do setInput (dropWhile isSpace es)
                                    return (reverse (a:rs))
            _ -> fail $ "Unable to parse JSON array: unterminated array: " ++ context ds


-- | Read a sequence of JSON labelled fields
readAssocs :: Char -> Char -> Char -> GetJSON [(String,JSValue)]
readAssocs start end sep = do
  zs <- getInput
  case dropWhile isSpace zs of
    c:cs | c == start -> case dropWhile isSpace cs of
            d:ds | d == end -> setInput (dropWhile isSpace ds) >> return []
            ds              -> setInput ds >> parsePairs []
    _ -> fail "Unable to parse JSON object: unterminated object"

  where parsePairs rs = rs `seq` do
          a  <- do k  <- do x <- readJSString ; case x of
                                JSString s -> return s
                                _          -> fail $ "Malformed JSON field labels: object keys must be quoted strings."
                   ds <- getInput
                   case dropWhile isSpace ds of
                       ':':es -> do setInput (dropWhile isSpace es)
                                    v <- readJSValue
                                    return (fromJSString k,v)
                       _      -> fail $ "Malformed JSON labelled field: " ++ context ds

          ds <- getInput
          case dropWhile isSpace ds of
            e : es | e == sep -> do setInput (dropWhile isSpace es)
                                    parsePairs (a:rs)
                   | e == end -> do setInput (dropWhile isSpace es)
                                    return (reverse (a:rs))
            _ -> fail $ "Unable to parse JSON object: unterminated sequence: "
                            ++ context ds

-- | Read one of several possible JS types
readJSValue :: GetJSON JSValue
readJSValue = do
  cs <- getInput
  case cs of
    '"' : _ -> readJSString
    '[' : _ -> readJSArray
    '{' : _ -> readJSObject
    't' : _ -> readJSBool
    'f' : _ -> readJSBool
    xs    | "null" `isPrefixOf` xs -> readJSNull
    (x:_) | x == '-' || isDigit x  -> JSRational <$> readJSRational

    xs -> fail $ "Malformed JSON: invalid token in this context " ++ context xs

-- | Top level JSON can only be Arrays or Objects
readJSTopType :: GetJSON JSValue
readJSTopType = do
  cs <- getInput
  case cs of
    '[' : _ -> readJSArray
    '{' : _ -> readJSObject
    _       -> fail "Invalid JSON: a JSON text a serialized object or array at the top level."

-- -----------------------------------------------------------------
-- | Writing JSON

-- | Show strict JSON top level types. Values not permitted
-- at the top level are wrapped in a singleton array.
showJSTopType :: JSValue -> ShowS
showJSTopType (JSArray a)    = showJSArray a
showJSTopType (JSObject o)   = showJSObject o
showJSTopType x              = showJSTopType $ JSArray [x]

-- | Show JSON values
showJSValue :: JSValue -> ShowS
showJSValue (JSNull)       = showJSNull
showJSValue (JSBool b)     = showJSBool b
showJSValue (JSRational r) = showJSRational r
showJSValue (JSArray a)    = showJSArray a
showJSValue (JSString s)   = showJSString s
showJSValue (JSObject o)   = showJSObject o

-- | Write the JSON null type
showJSNull :: ShowS
showJSNull = showString "null"

-- | Write the JSON Bool type
showJSBool :: Bool -> ShowS
showJSBool True  = showString "true"
showJSBool False = showString "false"

-- | Write the JSON String type
showJSString :: JSString -> ShowS
showJSString x xs = quote (encJSString x (quote xs))
  where
        quote = showChar '"'

-- | Show a Rational in JSON format
showJSRational :: Rational -> ShowS
showJSRational r | denominator r == 1 = shows $ numerator r
                 | otherwise = if isInfinite x || isNaN x then showJSNull
                                                          else shows x
                     where x :: Double
                           x = realToFrac r

-- | Show a list in JSON format
showJSArray :: [JSValue] -> ShowS
showJSArray = showSequence '[' ']' ','

-- | Show an association list in JSON format
showJSObject :: JSObject JSValue -> ShowS
showJSObject = showAssocs '{' '}' ',' . fromJSObject

-- | Show a generic sequence of pairs in JSON format
showAssocs :: Char -> Char -> Char -> [(String,JSValue)] -> ShowS
showAssocs start end sep xs rest = start : go xs
  where
  go [(k,v)]     = '"' : encJSString (toJSString k)
                            ('"' : ':' : showJSValue v (go []))
  go ((k,v):kvs) = '"' : encJSString (toJSString k)
                            ('"' : ':' : showJSValue v (sep : go kvs))
  go []          = end : rest

-- | Show a generic sequence in JSON format
showSequence :: Char -> Char -> Char -> [JSValue] -> ShowS
showSequence start end sep xs rest = start : go xs
  where
  go [y]        = showJSValue y (go [])
  go (y:ys)     = showJSValue y (sep : go ys)
  go []         = end : rest

encJSString :: JSString -> ShowS
encJSString jss ss = go (fromJSString jss)
  where
  go s1 =
    case s1 of
      (x   :xs) | x < '\x20' -> '\\' : encControl x (go xs)
      ('"' :xs)              -> '\\' : '"'  : go xs
      ('\\':xs)              -> '\\' : '\\' : go xs
      (x   :xs)              -> x    : go xs
      ""                     -> ss

  encControl x xs = case x of
    '\b' -> 'b' : xs
    '\f' -> 'f' : xs
    '\n' -> 'n' : xs
    '\r' -> 'r' : xs
    '\t' -> 't' : xs
    _ | x < '\x10'   -> 'u' : '0' : '0' : '0' : hexxs
      | x < '\x100'  -> 'u' : '0' : '0' : hexxs
      | x < '\x1000' -> 'u' : '0' : hexxs
      | otherwise    -> 'u' : hexxs
      where hexxs = showHex (fromEnum x) xs

