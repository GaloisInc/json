--------------------------------------------------------------------
-- |
-- Module    : Text.JSON.Types
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

module Text.JSON.Types (

    -- * JSON Types
    JSValue(..)

    -- * Wrapper Types
  , JSString
  , toJSString
  , fromJSString

  , JSObject
  , toJSObject
  , fromJSObject

  ) where

import Data.Char
import Data.Ratio

--
-- | JSON values
--
-- The type to which we encode Haskell values. There's a set
-- of primitives, and a couple of heterogenous collection types.
--
-- Objects:
--
-- An object structure is represented as a pair of curly brackets
-- surrounding zero or more name\/value pairs (or members).  A name is a
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
data JSValue
    = JSNull
    | JSBool     !Bool
    | JSRational !Rational
    | JSString   JSString
    | JSArray    [JSValue]
    | JSObject   (JSObject JSValue)
    deriving (Show, Read, Eq, Ord)

-- | Strings can be represented a little more efficiently in JSON
newtype JSString   = JSONString { fromJSString :: String }
    deriving (Eq, Ord, Show, Read)

-- | Turn a Haskell string into a JSON string.
toJSString :: String -> JSString
toJSString = JSONString

-- | As can association lists
newtype JSObject e = JSONObject { fromJSObject :: [(String, e)] }
    deriving (Eq, Ord, Show, Read)

-- | Make JSON object out of an association list.
toJSObject :: [(String,a)] -> JSObject a
toJSObject = JSONObject


