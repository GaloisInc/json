{-# OPTIONS_GHC -fglasgow-exts  #-}
module Main where

import Text.JSON

import Parallel

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import qualified Control.Exception as C (catch,evaluate)
import Control.Monad
import Foreign
import System.Environment
import System.IO
import System.IO.Unsafe
import Data.Word
import Data.Int

import Test.QuickCheck hiding (test)
import QuickCheckUtils
import Debug.Trace
import Text.Printf

import Data.IntSet          ( IntSet )
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Sequence as Seq
import qualified Data.Map as M
import qualified Data.IntMap as I

------------------------------------------------------------------------
-- low level ones:

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    s <- getArgs
    let n = if null s then 100 else read (head s)
        k = doIt n

    k basics
    k atomicCharacterTypes
    k numbers
    k listlikes
    k containers
    k sumtypes
    k products


doIt n (s,x) = putStrLn (" *** " ++ s) >> pRun 2 n x


type T a = a -> Property
type B a = a -> Bool

p :: Testable a => a -> Int -> IO String
p = pNon

test :: forall a. (Show a,Arbitrary a, Eq a, JSON a) => a -> Property
test _ = forAll (arbitrary :: Gen a) $ \a ->
                Ok a == decode (encode a)

instance Arbitrary JSString where
    arbitrary = liftM toJSString arbitrary
    coarbitrary = undefined

instance (Ord e, Arbitrary e) => Arbitrary (JSObject e) where
    arbitrary   = do
            ks <- arbitrary
            vs <- arbitrary
            return . toJSObject . M.toList . M.fromList . zip ks $ vs

    coarbitrary = undefined

------------------------------------------------------------------------

-- tests :: [(String, Int -> IO String)]
basics = ("Basic types",
                [("Bool",        p (test :: T Bool ))
                ,("()",          p (test :: T () ))
                ]
         )

        -- atomic character types

atomicCharacterTypes =
    ("Atomic string types",

    [("String",      p (test :: T JSString  ))
    ,("Strict ByteString",       p (test :: T S.ByteString ))
    ,("Lazy ByteString",         p (test :: T L.ByteString ))
    ,("Char",        p (test :: T Char ))
    ]
    )

        -- basic numeric types
numbers =
    ("Numeric types",
        [("Integer",     p (test :: T Integer ))
        ,("Int",         p (test :: T Int ))
        ,("Word",        p (test :: T Word ))

        -- words

        ,("Word8",        p (test :: T Word8    ))
        ,("Word16",       p (test :: T Word16   ))
        ,("Word32",       p (test :: T Word32   ))
        ,("Word64",       p (test :: T Word64   ))

        -- integers

        ,("Int8",         p (test :: T Int8     ))
        ,("Int16",        p (test :: T Int16    ))
        ,("Int32",        p (test :: T Int32    ))
        ,("Int64",        p (test :: T Int64    ))

        -- rationals

        ,("Double",         p (test :: T Double))
        ,("Float",          p (test :: T Float))
    ])

        -- lists

listlikes =
    ("List like types",
        [("[()]",           p (test :: T [()]))
        ,("[Int]",          p (test :: T [Int]))
        ,("[Bool]",         p (test :: T [Bool]))
        ,("[Integer]",      p (test :: T [Integer]))
        ,("[Int]",          p (test :: T [Int]))
        ,("[Word]",         p (test :: T [Word]))
        ,("[S.ByteString]", p (test :: T [S.ByteString]               ))
        ,("[L.ByteString]", p (test :: T [L.ByteString]               ))

    ])
        -- containers

containers =
    ("Container types",
        [("IntSet",         p (test :: T IntSet ))
        ,("Map String Int", p (test :: T (M.Map String Int)      ))
        ,("Map Int String", p (test :: T (M.Map Int String)      ))

--      ,("Maybe Bool",  p (test :: T (Maybe Bool) ))
--      ,("Rational",   p (test :: T Rational ))

        ]
    )

sumtypes =
    ("Sum types",
        [("Ordering",         p (test :: T Ordering))
        ,("Maybe Int",        p (test :: T (Maybe Int)))
        ,("Maybe String",        p (test :: T (Maybe String)))
        ,("Either Bool String",        p (test :: T (Either Bool String)))
        ,("Either Int (Either Int Word32)",
                    p (test :: T (Either Int (Either Int Word32))))
    ])

products =
    ("Products",
        [("((),())",
            p (test :: T ((),())
         ))

        ,("(Bool,Int)",
            p (test :: T (Bool,Int)
            ))

        ,("(Bool,(Int, String))",
            p (test :: T (Bool,(Int,String))
            ))

        ,("(Maybe String,(Either Int Bool, String))",
            p (test :: T (Bool,(Either Int Bool,String))
            ))

        ,("(Bool,Int,String)",
            p (test :: T (Bool,Int,String)
            ))

        ,("(Bool,Int,String,Char)",
            p (test :: T (Bool,Int,String,Char)
            ))

         ]

    )
