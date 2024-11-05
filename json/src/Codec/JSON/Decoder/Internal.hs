{-# LANGUAGE BangPatterns
           , DataKinds
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , KindSignatures
           , OverloadedStrings
           , PatternSynonyms #-}

module Codec.JSON.Decoder.Internal
  ( Bitmask
      ( Bitmask
      , EmptyBitmask
      , ObjectBit
      , ArrayBit
      , NumberBit
      , StringBit
      , BooleanBit
      , NullBit
      )

  , Path (..)

  , Gravity (..)
  , Error (..)

  , K (..)

  , Decoder (..)
  , KeyDecoder (..)

  , Pair (..)
  , Element (..)
  ) where

import           Data.JSON.Internal

import           Data.Bits
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Functor.Alt
import           Data.Text (Text)
import           Data.Word
import           Parser.Lathe as Lathe



-- | Set of six JSON value types that can be tried by any particular parser.
newtype Bitmask = Bitmask Word8

instance Show Bitmask where
  showsPrec _ (Bitmask mask) =
      showChar '{'
    . case checks of
        []   -> id
        x:xs -> showString x . foldr (\a acc -> showChar ',' . showString a . acc) id xs

    . showChar '}'
    where
      check (Bitmask b) str = if (mask .&. b) /= 0
                                then (:) str
                                else id

      checks =
         check ObjectBit  "object"
       . check ArrayBit   "array"
       . check NumberBit  "number"
       . check StringBit  "string"
       . check BooleanBit "boolean"
       . check NullBit    "null"
       $ []

instance Semigroup Bitmask where
  Bitmask a <> Bitmask b = Bitmask $ a .|. b

pattern EmptyBitmask
      , ObjectBit
      , ArrayBit
      , NumberBit
      , StringBit
      , BooleanBit
      , NullBit
     :: Bitmask
pattern EmptyBitmask = Bitmask 0x00
pattern ObjectBit    = Bitmask 0x01
pattern ArrayBit     = Bitmask 0x02
pattern NumberBit    = Bitmask 0x04
pattern StringBit    = Bitmask 0x08
pattern BooleanBit   = Bitmask 0x10
pattern NullBit      = Bitmask 0x20



-- | Subset of JSONPath used for error reporting, represented as a snoc-list.
data Path = -- | Object pair.
            Key
              !Path
              !JSONKey             -- ^ Name of the pair.

            -- | Array element.
          | Index
              !Path
              {-# UNPACK #-} !Word -- ^ Index of the element. Starts at @0@.

            -- | Root node identifier.
          | Root

instance Show Path where
  showsPrec _ p =
    case p of
      Key p' (JSONKey (JSON k)) ->
        let -- Hides the double quotes on both sides
            k' = JSON (LB.dropEnd 1 $ LB.drop 1 k)

        in shows p' . showChar '.' . shows k'

      Index p' i -> shows p' . showChar '[' . shows i . showChar ']'
      Root       -> showChar '$'



-- | Whether the error can be recovered from.
data Gravity = Benign -- ^ Personal judgement.
             | Fatal  -- ^ JSON data itself is malformed.
               deriving Show

-- | Kinds of errors known to the parser.
data Error = -- | Parser does not serve this JSON type.
              Mismatch
                {-# UNPACK #-} !Bitmask -- ^ Set of JSON types that were tried.
                {-# UNPACK #-} !K       -- ^ JSON type in question.

              -- | Input could not be converted properly.
            | Malformed
                {-# UNPACK #-} !Gravity
                !String                 -- ^ Error description.

              -- | Input ended abruptly.
            | AbruptEnd

instance Show Error where
  showsPrec _ x =
    case x of
      Mismatch bits k -> showString "\"Expected " . expandS bits
                           . showString ", but found " . convertS k . showChar '"'
      Malformed _ msg -> shows msg
      AbruptEnd       -> showString "\"Unexpected end of input\""
    where
      objectS  = showString "object"
      arrayS   = showString "array"
      numberS  = showString "number"
      stringS  = showString "string"
      booleanS = showString "boolean"
      nullS    = showString "null"

      convertS k =
        case k of
          O -> objectS
          A -> arrayS
          N -> numberS
          S -> stringS
          T -> booleanS
          F -> booleanS
          X -> nullS

      expandS (Bitmask mask) = sift checks
        where
          sift        []  = showString "???"
          sift     (a:[]) = a
          sift   (a:b:[]) = a . showString " or " . b
          sift (a:b:c:ds) = a . showString ", " . sift (b:c:ds)

          checks =
              check ObjectBit  objectS
            . check ArrayBit   arrayS
            . check NumberBit  numberS
            . check StringBit  stringS
            . check BooleanBit booleanS
            . check NullBit    nullS
            $ []

          check (Bitmask b) str = if (mask .&. b) /= 0
                                    then (:) str
                                    else id



-- | First byte of a JSON value.
data K = O -- ^ Object.
       | A -- ^ Array.
       | N -- ^ Number.
       | S -- ^ String.
       | T -- ^ True.
       | F -- ^ False.
       | X -- ^ Null.
         deriving Show



-- | The decoder type, parametrized by the return type @a@.
newtype Decoder a =
          Decoder { runDecoder :: Path -> Bitmask -> K -> Parser (Path, Error) a }

instance Functor Decoder where
  fmap f (Decoder p) =
    Decoder $ \path bits k -> f <$> p path bits k

-- | '(<!>)': references to all new input consumed by the left decoder are
--   kept until it completes.
instance Alt Decoder where
  Decoder a <!> Decoder b =
    Decoder $ \path bits k ->
      a path bits k `catch` \ro@(_, e) ->
        case e of
          Mismatch bits' _    -> b path bits' k
          Malformed gravity _ -> case gravity of
                                   Benign -> b path bits k
                                   Fatal  -> err ro
          AbruptEnd           -> err ro



-- | Object pair name decoder type
newtype KeyDecoder a = KeyDecoder (Decoder a)
                       deriving newtype Functor

-- | '(<!>)': references to all new input consumed by the left decoder are
--   kept until it completes.
instance Alt KeyDecoder where
  KeyDecoder a <!> KeyDecoder b = KeyDecoder (a <!> b)



-- | Class of JSON object parsers.
class Pair f where
  -- | Ditto '(Codec.JSON.Decoder..:)'.
  pair :: Text -> Decoder a -> f a

  -- | Decode an optional JSON value at the given key.
  --
  --   Returns 'Nothing' if the key is not present in the supplied JSON object.
  pairMaybe :: Text -> Decoder (Maybe a) -> f (Maybe a)



-- | Class of JSON array parsers.
class Element f where
  -- | Decode a JSON value at the current position.
  element :: Decoder a -> f a
