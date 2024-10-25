{-# LANGUAGE DataKinds
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , KindSignatures
           , OverloadedStrings #-}

module Data.JSON.Internal
  ( JSON (..)
  , JSONKey (..)
  ) where

import           Data.ByteString.Lazy as LB (ByteString)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding



-- | Raw JSON value.
newtype JSON = JSON LB.ByteString

instance Show JSON where
  showsPrec _ (JSON ro) =
    showString . LT.unpack $ case decodeUtf8' ro of
                               Right t -> t
                               Left _  -> "<malformed UTF-8>"



-- | Raw JSON object pair name.
newtype JSONKey = JSONKey JSON
                  deriving newtype Show
