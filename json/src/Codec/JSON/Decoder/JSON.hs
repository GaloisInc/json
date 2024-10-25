{-# LANGUAGE BangPatterns
           , OverloadedStrings #-}

module Codec.JSON.Decoder.JSON
  ( json
  , jsonObject
  , jsonArray
  , jsonNumber
  , jsonString
  , jsonBoolean
  , jsonNull

  , json'
  , jsonObject'
  , jsonArray'
  , jsonString'
  ) where

import           Codec.JSON.Decoder.JSON.Internal
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Literals.Internal
import           Codec.JSON.Decoder.String.Internal
import           Data.JSON.Internal

import           Parser.Lathe



-- | Validate a JSON object and copy it verbatim.
--
--   Surrogate Unicode code points in JSON strings are treated as valid scalar values.
jsonObject :: Decoder JSON
jsonObject = jsonObject_ Preserve_

-- | Validate a JSON object and copy it verbatim.
--
--   Surrogate Unicode code points in JSON strings are treated as invalid input.
jsonObject' :: Decoder JSON
jsonObject' = jsonObject_ Complain_

{-# INLINE jsonObject_ #-}
jsonObject_ :: SurrogateHandling_ -> Decoder JSON
jsonObject_ handl =
  Decoder $ \path bits k ->
    case k of
      O -> do
        (ro, _) <- match $ jsonObjectP handl path
        pure (JSON ro)

      _ ->
        let !bits' = bits <> ObjectBit
        in err (path, Mismatch bits' k)


-- | Validate a JSON array and copy it verbatim.
--
--   Surrogate Unicode code points in JSON strings are treated as valid scalar values.
jsonArray :: Decoder JSON
jsonArray = jsonArray_ Preserve_

-- | Validate a JSON array and copy it verbatim.
--
--   Surrogate Unicode code points in JSON strings are treated as invalid input.
jsonArray' :: Decoder JSON
jsonArray' = jsonArray_ Complain_

{-# INLINE jsonArray_ #-}
jsonArray_ :: SurrogateHandling_ -> Decoder JSON
jsonArray_ handl =
  Decoder $ \path bits k ->
    case k of
      A -> do
        (ro, _) <- match $ jsonArrayP handl path
        pure (JSON ro)

      _ ->
        let !bits' = bits <> ArrayBit
        in err (path, Mismatch bits' k)


-- | Validate a JSON number and copy it verbatim.
jsonNumber :: Decoder JSON
jsonNumber =
  Decoder $ \path bits k ->
    case k of
      N -> do
        (ro, _) <- match $ jsonNumberP path
        pure (JSON ro)

      _ ->
        let !bits' = bits <> NumberBit
        in err (path, Mismatch bits' k)


-- | Validate a JSON string and copy it verbatim.
--
--   Surrogate Unicode code points are treated as valid scalar values.
jsonString :: Decoder JSON
jsonString = jsonString_ Preserve_

-- | Validate a JSON string and copy it verbatim.
--
--   Surrogate Unicode code points are treated as invalid input.
jsonString' :: Decoder JSON
jsonString' = jsonString_ Complain_

{-# INLINE jsonString_ #-}
jsonString_ :: SurrogateHandling_ -> Decoder JSON
jsonString_ handl =
  Decoder $ \path bits k ->
    case k of
      S -> do
        (ro, _) <- match $ jsonStringP handl path
        pure (JSON ro)

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)


-- | Validate a JSON boolean and copy it verbatim.
jsonBoolean :: Decoder JSON
jsonBoolean =
  Decoder $ \path bits k -> do
    case k of
      T -> JSON "true" <$ trueP path
      F -> JSON "false" <$ falseP path
      _ -> let !bits' = bits <> BooleanBit
           in err (path, Mismatch bits' k)


-- | Validate a JSON null and copy it verbatim.
jsonNull :: Decoder JSON
jsonNull =
  Decoder $ \path bits k -> do
    case k of
      X -> JSON "null" <$ nullP path
      _ -> let !bits' = bits <> NullBit
           in err (path, Mismatch bits' k)



-- | Validate a JSON value and copy it verbatim.
--
--   Surrogate Unicode code points in JSON strings are treated as valid scalar values.
json :: Decoder JSON
json = json_ Preserve_

-- | Validate a JSON value and copy it verbatim.
--
--   Surrogate Unicode code points in JSON strings are treated as invalid input.
json' :: Decoder JSON
json' = json_ Complain_

{-# INLINE json_ #-}
json_ :: SurrogateHandling_ -> Decoder JSON
json_ handl =
  Decoder $ \path _ k -> do
    (ro, _) <- match $ jsonP handl path k
    pure (JSON ro)
