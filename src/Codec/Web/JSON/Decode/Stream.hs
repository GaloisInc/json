module Codec.Web.JSON.Decode.Stream where

import           Data.Attoparsec.ByteString



-- | Streaming JSON decoder.
--
--   Returns a stream of @a@ outputs, ending with a single @r@ output.
data Stream a r = Yield a (Stream a r)
                | Parse (Parser (Stream a r))
                | Return r
