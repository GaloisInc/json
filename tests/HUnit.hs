{-# OPTIONS -fglasgow-exts #-}

import Text.JSON
import Test.HUnit
import System.Exit (exitFailure)
import Control.Monad (when)
import System.IO
import Data.Either
import qualified Data.Map as M

isError (Error _) = True
isError _        = False


main = do counts <- runTestTT tests
          when (errors counts > 0 || failures counts > 0) exitFailure

tests = TestList
    [shouldFail "non-array top level"         "fail1" (undefined :: String)
    ,shouldFail "unclosed array"              "fail2" (undefined :: JSValue)
    ,shouldFail "object keys must be quoted"  "fail3" (undefined :: JSValue)
    ,shouldFail "extra comma"                 "fail4" (undefined :: JSValue)
    ,shouldFail "double extra comma"          "fail5" (undefined :: JSValue)
    ,shouldFail "missing value"               "fail6" (undefined :: JSValue)
    ,shouldFail "comma after close"           "fail7" (undefined :: JSValue)
    ,shouldFail "extra close"                 "fail8" (undefined :: JSValue)
    ,shouldFail "extra comma"                 "fail9" (undefined :: JSValue)
    ,shouldFail "extra value"                 "fail10" (undefined :: JSValue)
    ,shouldFail "illegal expression"          "fail11" (undefined :: JSValue)
    ,shouldFail "illegal expression"          "fail12" (undefined :: JSValue)
    ,shouldFail "numbers with leading zeroes" "fail13" (undefined :: JSValue)
    ,shouldFail "numbers in hex"              "fail14" (undefined :: JSValue)
    ,shouldFail "illegal backslash"           "fail15" (undefined :: JSValue)
    ,shouldFail "unquoted char"               "fail16" (undefined :: JSValue)
    ,shouldFail "illegal escape"              "fail17" (undefined :: JSValue)
    ,shouldPass "deep objects"                "fail18" (undefined :: JSValue)  -- depth is allowed to be limited, but why bother?
    ,shouldFail "missing colon"               "fail19" (undefined :: JSValue)
    ,shouldFail "double colon"                "fail20" (undefined :: JSValue)
    ,shouldFail "comma instead of colon"      "fail21" (undefined :: JSValue)
    ,shouldFail "colon intead of comma"       "fail22" (undefined :: JSValue)
    ,shouldFail "invalid token"               "fail23" (undefined :: JSValue)
    ,shouldFail "single quotes"               "fail24" (undefined :: JSValue)
    ,shouldFail "literal tabs"                "fail25" (undefined :: JSValue)
    ,shouldFail "tabs in strings"             "fail26" (undefined :: JSValue)
    ,shouldFail "newline in strings"          "fail27" (undefined :: JSValue)
    ,shouldFail "escaped newline in strings"  "fail28" (undefined :: JSValue)
    ,shouldFail "funny number"                "fail29" (undefined :: JSValue)
    ,shouldFail "funny number 2"              "fail30" (undefined :: JSValue)
    ,shouldFail "funny number 3"              "fail31" (undefined :: JSValue)
    ,shouldFail "unterminated array"          "fail32" (undefined :: JSValue)
    ,shouldFail "unterminated array"          "fail33" (undefined :: JSValue)

    , shouldPass "complex valid input 1"        "pass1"  (undefined :: JSValue)
    , shouldPass "complex valid input 2"        "pass2"  (undefined :: JSValue)
    , shouldPass "complex valid input 3"        "pass3"  (undefined :: JSValue)
    ]

------------------------------------------------------------------------

load n = readFile ("unit/" ++ n ++ ".json")

shouldFail :: JSON a => String -> String -> a -> Test
shouldFail  s n (x :: a) = TestLabel ("Should fail: " ++ s) $
  TestCase $ do
--  hPutStrLn stderr $ ("\t\tShould fail: " ++ s)
    s <- load n
    assert =<< case decodeStrict s :: Result a of
                    Ok _     -> return False
                    Error  s -> -- do hPrint stderr s
                                   return True


shouldPass :: JSON a => String -> String -> a -> Test
shouldPass  s n (x :: a) = TestLabel ("Should pass: " ++ s) $
  TestCase $ do
--  hPutStrLn stderr $ ("\t\tShould pass: " ++ s)
    s <- load n
    assert =<< case decodeStrict s :: Result a of
                    Ok _     -> return True
                    Error  s -> do hPrint stderr s
                                   return False

