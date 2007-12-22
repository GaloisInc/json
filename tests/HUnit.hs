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
    ,shouldFail "unclosed array"              "fail2" (undefined :: JSType)
    ,shouldFail "object keys must be quoted"  "fail3" (undefined :: JSType)
    ,shouldFail "extra comma"                 "fail4" (undefined :: JSType)
    ,shouldFail "double extra comma"          "fail5" (undefined :: JSType)
    ,shouldFail "missing value"               "fail6" (undefined :: JSType)
    ,shouldFail "comma after close"           "fail7" (undefined :: JSType)
    ,shouldFail "extra close"                 "fail8" (undefined :: JSType)
    ,shouldFail "extra comma"                 "fail9" (undefined :: JSType)
    ,shouldFail "extra value"                 "fail10" (undefined :: JSType)
    ,shouldFail "illegal expression"          "fail11" (undefined :: JSType)
    ,shouldFail "illegal expression"          "fail12" (undefined :: JSType)
    ,shouldFail "numbers with leading zeroes" "fail13" (undefined :: JSType)
    ,shouldFail "numbers in hex"              "fail14" (undefined :: JSType)
    ,shouldFail "illegal backslash"           "fail15" (undefined :: JSType)
    ,shouldFail "unquoted char"               "fail16" (undefined :: JSType)
    ,shouldFail "illegal escape"              "fail17" (undefined :: JSType)
    ,shouldPass "deep objects"                "fail18" (undefined :: JSType)  -- depth is allowed to be limited, but why bother?
    ,shouldFail "missing colon"               "fail19" (undefined :: JSType)
    ,shouldFail "double colon"                "fail20" (undefined :: JSType)
    ,shouldFail "comma instead of colon"      "fail21" (undefined :: JSType)
    ,shouldFail "colon intead of comma"       "fail22" (undefined :: JSType)
    ,shouldFail "invalid token"               "fail23" (undefined :: JSType)
    ,shouldFail "single quotes"               "fail24" (undefined :: JSType)
    ,shouldFail "literal tabs"                "fail25" (undefined :: JSType)
    ,shouldFail "tabs in strings"             "fail26" (undefined :: JSType)
    ,shouldFail "newline in strings"          "fail27" (undefined :: JSType)
    ,shouldFail "escaped newline in strings"  "fail28" (undefined :: JSType)
    ,shouldFail "funny number"                "fail29" (undefined :: JSType)
    ,shouldFail "funny number 2"              "fail30" (undefined :: JSType)
    ,shouldFail "funny number 3"              "fail31" (undefined :: JSType)
    ,shouldFail "unterminated array"          "fail32" (undefined :: JSType)
    ,shouldFail "unterminated array"          "fail33" (undefined :: JSType)

    , shouldPass "complex valid input 1"        "pass1"  (undefined :: JSType)
    , shouldPass "complex valid input 2"        "pass2"  (undefined :: JSType)
    , shouldPass "complex valid input 3"        "pass3"  (undefined :: JSType)
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

