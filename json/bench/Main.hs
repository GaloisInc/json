{-# LANGUAGE ApplicativeDo
           , BangPatterns
           , OverloadedStrings
           , TemplateHaskell #-}

module Main where

import qualified Codec.JSON.Decode as JSON
import qualified Codec.JSON.Decode.TH as JSON
import qualified Codec.JSON.Raw as JSON
import qualified Data.Aeson as Aeson

import           Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.Coerce
import           Data.Text (Text)
import           Test.Tasty.Bench



data Movie = Movie !Text !Int !(Maybe Text)
             deriving (Show, Eq)

instance NFData Movie where
  rnf !_ = ()

jsonMovies :: JSON.Decoder [Movie]
jsonMovies =
  JSON.list $
    JSON.pairsM $ do
      title <- "title" JSON..: JSON.text
      year  <- "year"  JSON..: JSON.int
      href  <- "href"  JSON.?: JSON.text
      pure $! Movie title year href

jsonMoviesA :: JSON.Decoder [Movie]
jsonMoviesA =
  JSON.list $
    JSON.pairsA $ do
      title <- "title" JSON..: JSON.text
      year  <- "year"  JSON..: JSON.int
      href  <- "href"  JSON.?: JSON.text
      pure $ Movie title year href

jsonMoviesQ :: JSON.Decoder [Movie]
jsonMoviesQ =
  JSON.list $
    $$( JSON.pairsQ $ do
          [|| Movie ||]
            JSON.|$| "title" JSON..| [|| JSON.text ||]
            JSON.|*| "year"  JSON..| [|| JSON.int ||]
            JSON.|*| "href"  JSON.?| [|| JSON.text ||]
      )



instance Aeson.FromJSON Movie where
  parseJSON =
    Aeson.withObject "Movie" $ \o -> do
      title <- o Aeson..: "title"
      year  <- o Aeson..: "year"
      href  <- o Aeson..:? "href"
      pure $! Movie title year href



main :: IO ()
main = do
  defaultMain
    [ env (L.fromStrict <$> B.readFile "/tmp/dump.json") $ \file ->
        bgroup "Epic"
          [ bench "JSON (naive)" $
              nf ( \f -> let (_, ei) = JSON.decode jsonMovies f
                         in case ei of
                              Right a -> a
                              Left e  -> error "no"
                 ) file

          , bench "JSON (Applicative)" $
              nf ( \f -> let (_, ei) = JSON.decode jsonMoviesA f
                         in case ei of
                              Right a -> a
                              Left e  -> error "no"
                 ) file

          , bench "JSON (Applicative/TH)" $
              nf ( \f -> let (_, ei) = JSON.decode jsonMoviesQ f
                         in case ei of
                              Right a -> a
                              Left e  -> error "no"
                 ) file

          , bench "Aeson" $
              nf ( \f -> case Aeson.eitherDecode f of
                           Right a -> a :: [Movie]
                           Left e  -> error "no"
                 ) file
          ]
    ]
