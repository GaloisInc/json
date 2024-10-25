{-# LANGUAGE BangPatterns
           , OverloadedStrings #-}

module Main
  ( main
  ) where

import           Codec.JSON.Decoder
import           Codec.JSON.Decoder.Stream
import           Codec.JSON.Decoder.Stream.Internal
import           Codec.JSON.Decoder.Unsafe
import           Data.JSON

import           Data.ByteString.Builder as Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Parser.Lathe
import           System.Exit
import           System.IO



main :: IO ()
main = do
  chunk0 <- B.hGet stdin defaultChunkSize
  end0 <- hIsEOF stdin
  let more0 = if end0
                then End
                else More

  unroll more0 . stream (pretty noIndent First) $ prepare 0 chunk0 LB.empty more0



unroll :: More -> Stream Action Partial (Blank, Either (Path, Error) Place) -> IO ()
unroll more0 x =
  case x of
    Yield a y -> do
      LB.hPutStr stdout . toLazyByteString $ buildAction a
      unroll more0 y

    Effect part0 -> go more0 part0
      where
        go more part =
          case part of
            Partial resupply ->
              case more of
                End  -> go End $ resupply EndOfInput
                More -> do
                  chunk <- B.hGet stdin defaultChunkSize
                  end <- hIsEOF stdin
                  let more' = if end
                                then End
                                else More

                  go more' $ resupply (Supply chunk)

            Done y -> unroll more y

    Return (blank', ei) ->
      case ei of
        Right _          -> LB.hPutStr stdout . toLazyByteString $ Builder.word8 0x0A
        Left (path, e) -> do
          let Scrap i _ _ = scrap blank'

          LB.hPutStr stdout $
            toLazyByteString $
              buildNewline
                <> Builder.byteString "Error at byte "
                <> int64Dec i
                <> Builder.byteString ", "
                <> stringUtf8 (show path)
                <> Builder.byteString ": "
                <> stringUtf8 (show e)
                <> buildNewline

          exitWith $ ExitFailure 1


buildNewline :: Builder
buildNewline =
  case nativeNewline of
    CRLF -> Builder.word16BE 0x0D0A
    LF   -> Builder.word16BE 0x0A

buildAction :: Action -> Builder
buildAction x =
  case x of
    Open indent composite place ->
      let brace = Builder.word16BE $
                    case composite of
                      Object -> 0x7B20
                      Array  -> 0x5B20
      in case place of
           First -> buildNewline
                      <> buildIndent (bump indent)
                      <> brace

           Next  -> buildNewline
                      <> buildIndent indent
                      <> Builder.word16BE 0x2C20
                      <> brace

    Close indent composite ->
      buildNewline
        <> buildIndent indent
        <> Builder.word8
             ( case composite of
                 Object -> 0x7D
                 Array  -> 0x5D
             )

    Element indent place raw ->
      case place of
        First -> Builder.lazyByteString (getRaw raw)
        Next  -> buildNewline
                   <> buildIndent indent
                   <> Builder.word16BE 0x2C20
                   <> Builder.lazyByteString (getRaw raw)

    Pair indent place key ->
      case place of
        First -> Builder.lazyByteString (getRaw $ unKey key)
                   <> Builder.word16BE 0x3A20

        Next  -> buildNewline
                   <> buildIndent (bump indent)
                   <> Builder.word16BE 0x2C20
                   <> Builder.lazyByteString (getRaw $ unKey key)
                   <> Builder.word16BE 0x3A20



newtype Indent = Indent Int
                 deriving Show

noIndent :: Indent
noIndent = Indent (-1)

bump :: Indent -> Indent
bump (Indent w) = Indent (w + 1)

buildIndent :: Indent -> Builder
buildIndent (Indent w)
  | w <= 0    = mempty
  | otherwise = Builder.word16BE 0x2020 <> buildIndent (Indent (w - 1))



pretty :: Indent -> Place -> Source Action Place
pretty !indent !place =
  Source $ \path _bits k ->
    Effect $
      prettyP indent place path k



data Composite = Object | Array
                 deriving Show

data Place = First | Next
             deriving Show

data Action = Pair
                {-# UNPACK #-} !Indent
                {-# UNPACK #-} !Place
                !JSONKey

            | Element
                {-# UNPACK #-} !Indent
                {-# UNPACK #-} !Place
                !JSON

            | Open
                {-# UNPACK #-} !Indent
                {-# UNPACK #-} !Composite
                {-# UNPACK #-} !Place

            | Close
                {-# UNPACK #-} !Indent
                {-# UNPACK #-} !Composite
              deriving Show

prettyP
  :: Indent
  -> Place
  -> Path
  -> K
  -> Parser (Path, Error) (Stream Action (Parser (Path, Error)) Place)
prettyP !indent !place !path !k = do
  let single f = do
        raw <- runDecoder f path EmptyBitmask k
        pure . Yield (Element indent place raw)
             $ Return Next

      singleRawBoolean = single jsonBoolean

  case k of
    O -> pure $
           Yield (Open indent Object place) $
             Effect $ do
               source <-
                   sourceObjectP_ path
                     (\key place' ->
                        Source $ \path' bits' k' ->
                          Yield (Pair indent place' key) $
                            runSource (pretty (bump (bump indent)) First) path' bits' k'
                     ) First

               pure $ chainStream source
                        (\_ -> Yield (Close (bump indent) Object) $ Return Next)

    A -> pure $
           Yield (Open indent Array place) $
             Effect $ do
               source <- sourceArrayP path (\_ -> pretty (bump indent)) First
               pure $ chainStream source
                        (\_ -> Yield (Close (bump indent) Array) $ Return Next)

    N -> single jsonNumber
    S -> single jsonString
    T -> singleRawBoolean
    F -> singleRawBoolean
    X -> single jsonNull
