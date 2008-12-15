{-# LANGUAGE PatternGuards #-}
--------------------------------------------------------------------
-- |
-- Module    : Text.JSON.Generic
-- Copyright : (c) Lennart Augustsson, 2008
-- License   : BSD3
--
-- Maintainer:  Sigbjorn Finne <sof@galois.com>
-- Stability :  provisional
-- Portability: portable
--
-- JSON serializer and deserializer using Data.Generics.
-- The functions here handle algebraic data types and primitive types.
-- It uses the same representation as "Text.JSON" for "Prelude" types.
module Text.JSON.Generic 
    ( module Text.JSON
    , Data
    , Typeable
    , toJSON
    , fromJSON
    , encodeJSON
    , decodeJSON
    ) where

import Control.Monad.State
import Control.Monad.Identity
import Data.Char(toLower, toUpper)
import Data.Maybe
import Text.JSON
import Text.JSON.String
import Data.Generics
import Data.Word
import Data.Int

type T a = a -> JSValue

-- |Convert anything to a JSON value.
toJSON :: (Data a) => a -> JSValue
toJSON = generic
         `ext1Q` jList
         -- Use the standard encoding for all base types.
         `extQ` (showJSON :: T Integer)
         `extQ` (showJSON :: T Int)
         `extQ` (showJSON :: T Word8)
         `extQ` (showJSON :: T Word16)
         `extQ` (showJSON :: T Word32)
         `extQ` (showJSON :: T Word64)
         `extQ` (showJSON :: T Int8)
         `extQ` (showJSON :: T Int16)
         `extQ` (showJSON :: T Int32)
         `extQ` (showJSON :: T Int64)
         `extQ` (showJSON :: T Double)
         `extQ` (showJSON :: T Float)
         `extQ` (showJSON :: T Char)
         `extQ` (showJSON :: T String)
         -- Some algebraic types have weird encodings.
         `extQ` (showJSON :: T Bool)
         `extQ` (showJSON :: T ())
         `extQ` (showJSON :: T Ordering)
  where
        -- Lists are simply coded as arrays.
        jList vs = JSArray $ map toJSON vs

        -- Generic encoding of an algebraic data type.
        --   No constructor, so it must be an error value.  Code it anyway as JSNull.
        --   Elide a single constructor and just code the arguments.
        --   For multiple constructors, make an object with a field name that is the
        --   constructor (except lower case) and the data is the arguments encoded.
        generic a =
            case dataTypeRep (dataTypeOf a) of
                AlgRep []  -> JSNull
                AlgRep [c] -> encodeArgs c (gmapQ toJSON a)
                AlgRep _   -> encodeConstr (toConstr a) (gmapQ toJSON a)
                rep        -> error $ "toJSON: not AlgRep " ++ show rep ++ "(" ++ show (dataTypeOf a) ++ ")"

        -- Encode nullary constructor as a string.
        -- Encode non-nullary constructors as an object with the constructor
        -- name as the single field and the arguments as the value.
        -- Use an array if the are no field names, but elide singleton arrays,
        -- and use an object if there are field names.
        encodeConstr c [] = JSString $ toJSString $ constrString c
        encodeConstr c as = jsObject [(constrString c, encodeArgs c as)]

        constrString = mungeConstr . showConstr

        encodeArgs c = encodeArgs' (constrFields c)
        encodeArgs' [] [j] = j
        encodeArgs' [] js  = JSArray js
        encodeArgs' ns js  = jsObject $ zip (map mungeField ns) js

        -- Skip leading '_' in field name so we can use keywords etc. as field names.
        mungeField ('_':cs) = cs
        mungeField cs = cs

        -- Turn leading capital letter into lower case
        mungeConstr (c:cs) = toLower c : cs
        mungeConstr "" = ""

	jsObject :: [(String, JSValue)] -> JSValue
        jsObject = JSObject . toJSObject



type F a = Result a

-- |Convert a JSON value to anything (fails if the types do not match).
fromJSON :: (Data a) => JSValue -> Result a
fromJSON j = generic
             `ext1R` jList
             `extR` (value :: F Integer)
             `extR` (value :: F Int)
             `extR` (value :: F Word8)
             `extR` (value :: F Word16)
             `extR` (value :: F Word32)
             `extR` (value :: F Word64)
             `extR` (value :: F Int8)
             `extR` (value :: F Int16)
             `extR` (value :: F Int32)
             `extR` (value :: F Int64)
             `extR` (value :: F Double)
             `extR` (value :: F Float)
             `extR` (value :: F Char)
             `extR` (value :: F String)
             `extR` (value :: F Bool)
             `extR` (value :: F ())
             `extR` (value :: F Ordering)
  where value :: (JSON a) => Result a
        value = readJSON j

        jList :: (Data e) => Result [e]
        jList = case j of
                JSArray js -> mapM fromJSON js
                _ -> Error $ "fromJSON: Prelude.[] bad data: " ++ show j

        typ = dataTypeOf $ resType generic
        generic = case dataTypeRep typ of
                      AlgRep []  -> case j of JSNull -> return (error "Empty type"); _ -> Error $ "fromJSON: no-constr bad data"
                      AlgRep [_] -> decodeArgs (indexConstr typ 1) j
                      AlgRep _   -> do (c, j') <- getConstr typ j; decodeArgs c j'
                      rep        -> Error $ "fromJSON: " ++ show rep ++ "(" ++ show typ ++ ")"
        getConstr t (JSObject o) | [(s, j')] <- fromJSObject o = do c <- readConstr' t s; return (c, j')
        getConstr t (JSString js) = do c <- readConstr' t (fromJSString js); return (c, JSNull) -- handle nullare constructor
        getConstr _ _ = Error "fromJSON: bad constructor encoding"
        readConstr' t s = maybe (Error $ "fromJSON: unknown constructor: " ++ s ++ " " ++ show t) return .
                          readConstr t . unmungeConstr $ s

        decodeArgs c = decodeArgs' (numConstrArgs (resType generic) c) c
        decodeArgs' 0 c JSNull = construct c []
        decodeArgs' 1 c jd | null (constrFields c) = construct c [jd]
        decodeArgs' n c (JSArray js) | (n::Int) > 1 && null (constrFields c) = construct c js
        decodeArgs' _ c (JSObject o) | not (null (constrFields c)) = construct c (map snd $ fromJSObject o) -- FIXME: should use field names
        decodeArgs' _ c jd = Error $ "fromJSON: bad decodeArgs data " ++ show (c, jd)

        -- Build the value by stepping through the list of subparts.
        construct c = evalStateT $ fromConstrM f c
          where f :: (Data a) => StateT [JSValue] Result a
                f = do js <- get; case js of [] -> lift $ Error "construct: empty list"; j' : js' -> do put js'; lift $ fromJSON j'

        unmungeConstr (c:cs) = toUpper c : cs
        unmungeConstr "" = ""

        -- Count how many arguments a constructor has.  The value x is used to determine what type the constructor returns.
        numConstrArgs x c = execState (fromConstrM f c `asTypeOf` return x) 0
          where f = do modify (+1); return undefined

        resType :: Result a -> a
        resType _ = error "resType"



-- |Encode a value as a string.
encodeJSON :: (Data a) => a -> String
encodeJSON x = showJSValue (toJSON x) ""

-- |Decode a string as a value.
decodeJSON :: (Data a) => String -> a
decodeJSON s =
    case runGetJSON readJSValue s of
    Left msg -> error msg
    Right j ->
        case fromJSON j of
        Error msg -> error msg
        Ok x -> x
