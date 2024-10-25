{-| RFC 8259 JSON data type.
 -}

module Data.JSON
  ( -- * Itself
    JSON
  , getRaw

    -- ** Key
  , JSONKey
  , unKey
  ) where

import           Data.JSON.Internal

import           Data.ByteString.Lazy as LB (ByteString)



-- | Extract the lazy 'LB.ByteString' underlying the JSON value.
getRaw :: JSON -> LB.ByteString
getRaw (JSON ro) = ro



-- | Downgrade a raw JSON object pair name to a raw JSON value.
unKey :: JSONKey -> JSON
unKey (JSONKey ro) = ro
