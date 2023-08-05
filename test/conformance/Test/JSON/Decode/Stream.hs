{-# LANGUAGE OverloadedStrings #-}

module Test.JSON.Decode.Stream
  ( Test.JSON.Decode.Stream.stream
  ) where

import           Codec.Web.JSON.Decode as JSON

import qualified Data.ByteString.Char8 as BSC
import           Data.IORef
import           Data.Text (Text)
import           Test.Hspec



bouncer :: IORef Int -> IO BSC.ByteString
bouncer ref = do
  n <- readIORef ref
  case n of
    0  -> pure "[ [    1 "
    1  -> pure   ",    2 "
    2  -> pure   ",    3 "
    3  -> pure   ",    4 "
    4  -> pure   ",    5 "
    5  -> pure "] , [  6 "
    6  -> pure   ",    7 "
    7  -> pure   ",    8 "
    8  -> pure   ",    9 "
    9  -> pure   ",   10 "
    10 -> pure "] , [ 11 ,   12 "
    12 -> pure   ",   13 ,   14 "
    14 -> pure   ",   15 ] , [ 16 "
    16 -> pure   ",   17 ,   18 "
    18 -> pure   ",   19 ,   20 "
    20 -> pure "] , [ 21 "
    21 -> pure   ",   22 "
    22 -> pure   ",   23 "
    23 -> pure   ",   24 "
    24 -> pure   ",   25 "
    25 -> pure "] ]A"
    _  -> fail $ "Out of range: " <> show n



-- | Parses a two-dimensional array 'bouncer' provides,
--   consuming options 0 to 25 in sequence.
square :: Expectation
square = do
  -- The streamed tuple is (accumulator, first dimension, second dimension, array value)
  let input = flip streamArray 0 $ \acc0 n ->
                flip streamArray acc0 $ \acc1 m ->
                  JSON.stream $ (\i -> ((acc1, n, m, i), acc1 + 1)) <$> int

  ref <- newIORef 0

  let go :: Source IO BSC.ByteString (Int, Int, Int, Int) Int -> IO ()
      go s =
        case s of
          Step (acc, n, m, i) s' -> if acc == 5 * n + m
                                      then if i == acc + 1
                                             then do writeIORef ref i
                                                     go s'

                                             else fail $ "i /= acc + 1 " <> show (acc, i)

                                      else fail $ "acc /= 5 * n + m " <> show (acc, n, m)

          Effect e -> e >>= go

          Done r _ -> do i <- readIORef ref
                         if i /= 25
                           then fail $ "i /= 25 (" <> shows i ")"
                           else if r == "A"
                                  then pure ()
                                  else fail $ "Incorrect trail (" <> shows r ")"

          Failed r path msg -> fail $ show (Failure r path msg :: Result BSC.ByteString ())

  go $ sourceF input (bouncer ref)



weaver :: IORef Int -> IO BSC.ByteString
weaver ref = do
  n <- readIORef ref
  case n of
    0  -> pure   "{ \"foo\" : { \"foo\" : 1 , \"bar\" : 2 "
    2  -> pure               ", \"baz\" : 3 "
    3  -> pure "} , \"bar\" : { \"foo\" : 4 "
    4  -> pure               ", \"bar\" : 5 , \"baz\" : 6 "
    6  -> pure "} , \"baz\" : { \"foo\" : 7 "
    7  -> pure               ", \"bar\" : 8 "
    8  -> pure               ", \"baz\" : 9 "
    9  -> pure "  } }A"
    _  -> fail $ "Out of range: " <> show n



unweave :: Text -> Text -> Maybe Int
unweave "foo" "foo" = Just 1
unweave "foo" "bar" = Just 2
unweave "foo" "baz" = Just 3
unweave "bar" "foo" = Just 4
unweave "bar" "bar" = Just 5
unweave "bar" "baz" = Just 6
unweave "baz" "foo" = Just 7
unweave "baz" "bar" = Just 8
unweave "baz" "baz" = Just 9
unweave _     _     = Nothing



-- | Parses a two-dimensional array 'weaver' provides,
--   consuming options 0 to 9 in sequence.
mesh :: Expectation
mesh = do
  -- The streamed tuple is (accumulator, outer pair name, inner pair name, array value)
  let input = flip (streamObject textFoldName) 0 $ \acc0 n ->
                flip (streamObject textFoldName) acc0 $ \acc1 m ->
                  JSON.stream $ (\i -> ((acc1, n, m, i), acc1 + 1)) <$> int

  ref <- newIORef 0

  let go :: Source IO BSC.ByteString (Int, Text, Text, Int) Int -> IO ()
      go s =
        case s of
          Step (acc, n, m, i) s' ->
            case unweave n m of
              Just j ->
                if i == acc + 1
                  then if j == i
                         then do
                           writeIORef ref i
                           go s'

                         else fail $ "Unweave for " <> show (n, m) <> " is not " <> show i

                  else fail $ "Accumulator is " <> show acc <> ", and i is " <> show i

              _ -> fail $ "No unweave for " <> show (n, m)

          Effect e -> e >>= go

          Done r _ -> do i <- readIORef ref
                         if i /= 9
                           then fail $ "i /= 9 (" <> shows i ")"
                           else if r == "A"
                                  then pure ()
                                  else fail $ "Incorrect trail (" <> shows r ")"

          Failed r path msg -> fail $ show (Failure r path msg :: Result BSC.ByteString ())

  go $ sourceF input (weaver ref)



stream :: Spec
stream =
  describe "Stream" $ do
    describe "Array" $
      it "Square" $
        square

    describe "Object" $
      it "Mesh" $
        mesh
