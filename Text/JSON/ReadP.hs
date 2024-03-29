-- | Parse JSON values using the ReadP combinators.

module Text.JSON.ReadP
  ( p_value
  , p_null
  , p_boolean
  , p_array
  , p_string
  , p_object
  , p_number
  , p_js_string
  , p_js_object
  , module Text.ParserCombinators.ReadP
  ) where

import Text.JSON.Types
import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Char
import Numeric

token            :: ReadP a -> ReadP a
token p           = skipSpaces **> p

p_value          :: ReadP JSValue
p_value           =  (JSNull      <$$  p_null)
                 <||> (JSBool      <$$> p_boolean)
                 <||> (JSArray     <$$> p_array)
                 <||> (JSString    <$$> p_js_string)
                 <||> (JSObject    <$$> p_js_object)
                 <||> (JSRational False <$$> p_number)

p_null           :: ReadP ()
p_null            = token (string "null") >> return ()

p_boolean        :: ReadP Bool
p_boolean         = token
                      (  (True  <$$ string "true")
                     <||> (False <$$ string "false")
                      )

p_array          :: ReadP [JSValue]
p_array           = between (token (char '[')) (token (char ']'))
                  $ p_value `sepBy` token (char ',')

p_string         :: ReadP String
p_string          = between (token (char '"')) (char '"') (many p_char)
  where p_char    =  (char '\\' >> p_esc)
                 <||> (satisfy (\x -> x /= '"' && x /= '\\'))

        p_esc     =  ('"'   <$$ char '"')
                 <||> ('\\'  <$$ char '\\')
                 <||> ('/'   <$$ char '/')
                 <||> ('\b'  <$$ char 'b')
                 <||> ('\f'  <$$ char 'f')
                 <||> ('\n'  <$$ char 'n')
                 <||> ('\r'  <$$ char 'r')
                 <||> ('\t'  <$$ char 't')
                 <||> (char 'u' **> p_uni)

        p_uni     = check =<< count 4 (satisfy isHexDigit)
          where check x | code <= max_char  = return (toEnum code)
                        | otherwise         = pfail
                  where code      = fst $ head $ readHex x
                        max_char  = fromEnum (maxBound :: Char)

p_object         :: ReadP [(String,JSValue)]
p_object          = between (token (char '{')) (token (char '}'))
                  $ p_field `sepBy` token (char ',')
  where p_field   = (,) <$$> (p_string <** token (char ':')) <**> p_value

p_number         :: ReadP Rational
p_number          = readS_to_P safeRationalReads

-- reading into a Double with reads is safe for huge floating-point literals
-- this will allow all floating-point literals that are small enough to fit
-- into a Double (and are thus compatible with most other json implementations)
-- to be parsed here without opening us to oversized Rational allocations
safeRationalReads :: ReadS Rational
safeRationalReads str =
  case reads str of
    [(d,_)] | not (isInfinite (d :: Double)) -> readSigned readFloat str
    _ -> []

p_js_string      :: ReadP JSString
p_js_string       = toJSString <$$> p_string

p_js_object      :: ReadP (JSObject JSValue)
p_js_object       = toJSObject <$$> p_object

--------------------------------------------------------------------------------
-- XXX: Because ReadP is not Applicative yet...

(<**>)  :: ReadP (a -> b) -> ReadP a -> ReadP b
(<**>)   = ap

(**>)   :: ReadP a -> ReadP b -> ReadP b
(**>)    = (>>)

(<**)   :: ReadP a -> ReadP b -> ReadP a
m <** n  = do x <- m; _ <- n; return x

(<||>)  :: ReadP a -> ReadP a -> ReadP a
(<||>)   = (+++)

(<$$>)  :: (a -> b) -> ReadP a -> ReadP b
(<$$>)   = fmap

(<$$)   :: a -> ReadP b -> ReadP a
x <$$ m  = m >> return x

