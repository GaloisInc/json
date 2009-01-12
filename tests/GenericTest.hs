{-# LANGUAGE DeriveDataTypeable, ExtendedDefaultRules, EmptyDataDecls #-}
module Main where
import Text.JSON.Generic
import Data.Word
import Data.Int

data Foo = Foo { a :: Int, b :: Bool, c :: Baz } | None
    deriving (Typeable, Data, Show, Eq)

data Baz = Baz Int
    deriving (Typeable, Data, Show, Eq)

data Bar = Int :+: Int | Zero
    deriving (Typeable, Data, Show, Eq)

newtype New a = New a
    deriving (Typeable, Data, Show, Eq)

newtype Apples = Apples { noApples :: Int }
    deriving (Typeable, Data, Show, Eq)

data Record = Record { x :: Int, y :: Double, z :: Float, s :: String, t :: (Bool, Int) }
    deriving (Typeable, Data, Show, Eq)

rec = Record { x = 1, y = 2, z = 3.5, s = "hello", t = (True, 0) }

data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Typeable, Data, Show, Eq)

atree = build 4
  where build 0 = Leaf
        build 1 = Node Leaf 100 Leaf
        build n = Node (build (n-1)) n (build (n-2))

data Color = Red | Green | Blue
    deriving (Typeable, Data, Show, Eq, Enum)

from (Ok x) = x
from (Error s) = error s

viaJSON :: (Data a) => a -> a
viaJSON = from . fromJSON . toJSON

testJSON :: (Data a, Eq a) => a -> Bool
testJSON x = --x == viaJSON x
             x == decodeJSON (encodeJSON x)

tests = and [
    testJSON (1::Integer),
    testJSON (42::Int),
    testJSON (100::Word8),
    testJSON (-1000::Int64),
    testJSON (4.2::Double),
    testJSON (4.1::Float),
    testJSON True,
    testJSON 'q',
    testJSON "Hello, World\n",
    testJSON (Nothing :: Maybe Int),
    testJSON (Just "aa"),
    testJSON [],
    testJSON [1,2,3,4],
    testJSON (Left 1 :: Either Int Bool),
    testJSON (Right True :: Either Int Bool),
    testJSON (1,True),
    testJSON (1,2,True,'a',"apa",(4.5,99)),
    testJSON $ Baz 11,
    testJSON $ Foo 1 True (Baz 42),
    testJSON None,
    testJSON $ 2 :+: 3,
    testJSON Zero,
    testJSON $ New (2 :+: 3),
    testJSON rec,
    testJSON [LT,EQ,GT],
    testJSON atree,
    testJSON (),
    testJSON $ Apples 42,
    testJSON [Red .. Blue]
    ]

main :: IO ()
main = if tests then return () else error "Generic test failed"
