{-# LANGUAGE BangPatterns
           , GADTs
           , ScopedTypeVariables
           , UnboxedTuples #-}

module Codec.JSON.Decoder.Object
  ( radixTree
  , radixTree'

  , PairsM
  , pairsM

  , PairsA
  , pairsA
  ) where

import           Codec.JSON.Decoder.Composite.Internal
import           Codec.JSON.Decoder.JSON.Internal
import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Object.Internal
import           Codec.JSON.Decoder.String (lazyText)
import           Codec.JSON.Decoder.String.Internal
import qualified Codec.JSON.Encoder as Enc
import           Data.JSON.Internal

import           Control.Exception
import           Data.ByteString.Builder (toLazyByteString)
import           Data.Functor.Alt
import           Data.RadixTree.Word8.Strict (RadixTree)
import qualified Data.RadixTree.Word8.Strict as Radix
import           Data.Text (Text)
import qualified Data.Text as Strict (unpack)
import qualified Data.Text.Lazy as LT (Text)
import           GHC.Exts (Any)
import           Parser.Lathe
import           Unsafe.Coerce



-- | Decode a JSON object as a 'RadixTree' of opaque JSON values.
--
--   Keys are guaranteed to be encoded as well-formed UTF-8.
--
--   In presence of duplicate keys in the supplied JSON object prefers later entries.
--
--   Surrogate Unicode code points in JSON strings are treated as valid scalar values.
radixTree :: Decoder (RadixTree JSON)
radixTree =
  Decoder $ \path bits k ->
    case k of
      O -> parsePairsM Preserve_ path
      _ ->
        let !bits' = bits <> ObjectBit
        in err (path, Mismatch bits' k)

-- | Decode a JSON string as a 'RadixTree' of opaque JSON values.
--
--   Keys are guaranteed to be encoded as well-formed UTF-8.
--
--   In presence of duplicate keys in the supplied JSON object prefers later entries.
--
--   Surrogate Unicode code points in JSON strings  are treated as invalid input.
radixTree' :: Decoder (RadixTree JSON)
radixTree' =
  Decoder $ \path bits k ->
    case k of
      O -> parsePairsM Complain_ path
      _ ->
        let !bits' = bits <> ObjectBit
        in err (path, Mismatch bits' k)



-- | Monadic object parser.
newtype PairsM a = PairsM { runPairsM :: Path -> RadixTree JSON -> Either (Path, Error) a }

instance Functor PairsM where
  fmap f (PairsM p) =
    PairsM $ \path tree ->
      f <$> p path tree

instance Apply PairsM where
  PairsM g <.> PairsM b =
    PairsM $ \path tree ->
      g path tree <*> b path tree

instance Applicative PairsM where
  pure a = PairsM $ \_ _ -> Right a

  (<*>) = (<.>)

instance Monad PairsM where
  PairsM p >>= m =
    PairsM $ \path tree -> do
      a <- p path tree
      runPairsM (m a) path tree

instance Alt PairsM where
  PairsM l <!> PairsM r =
    PairsM $ \path tree -> do
      case l path tree of
        Right a -> Right a
        Left _  -> r path tree

instance Pair PairsM where
  pair key (Decoder parser) =
    PairsM $ \path tree ->
      let rawKey = JSONKey (JSON (toLazyByteString . Enc.encode $ Enc.text key))
      in case lookupText key tree of
           Just (JSON ro) -> snd $ parse ( do k <- nextValueP Root
                                              parser (Key path rawKey) EmptyBitmask k
                                         ) ro

           Nothing       -> Left ( path
                                 , Malformed Benign $
                                     showString "Key " $ shows (Strict.unpack key)
                                                           " not found"
                                 )

  pairMaybe key (Decoder parser) =
    PairsM $ \path tree ->
      let rawKey = JSONKey (JSON (toLazyByteString . Enc.encode $ Enc.text key))
      in case lookupText key tree of
           Just (JSON ro) -> snd $ parse ( do k <- nextValueP Root
                                              parser (Key path rawKey) EmptyBitmask k
                                         ) ro

           Nothing       -> Right Nothing



findText :: a -> Text -> RadixTree a -> a
findText d txt = Radix.find d (Radix.feedText txt)

insertLazyText :: LT.Text -> a -> RadixTree a -> RadixTree a
insertLazyText txt raw = Radix.insert (Radix.feedLazyText txt) raw



-- | Parse a JSON object free-form.
--
--   In presence of duplicate keys in the supplied JSON object prefers later entries.
--
--   References to all input underlying the JSON object are kept until this
--   decoder completes.
pairsM :: PairsM a -> Decoder a
pairsM (PairsM p) =
  Decoder $ \path bits k ->
    case k of
      O -> do
        r <- parsePairsM Preserve_ path
        case p path r of
          Right a -> pure a
          Left e  -> err e

      _ ->
        let !bits' = bits <> ObjectBit
        in err (path, Mismatch bits' k)



parsePairsM :: SurrogateHandling_ -> Path -> Parser (Path, Error) (RadixTree JSON)
parsePairsM handl path = do
  skipEndOr1
  nonempty <- firstKeyP path
  if nonempty
    then go Radix.empty
    else pure Radix.empty
  where
    go !r = do
      key <- runDecoder lazyText path EmptyBitmask S
      nextColonP path
      k <- nextValueP path
      let rawKey = JSONKey (JSON (toLazyByteString . Enc.encode $ Enc.lazyText key))
      (raw, _) <- match $ jsonP handl (Key path rawKey) k
      let !r' = insertLazyText key (JSON raw) r

      more <- nextPairP path
      if more
        then do
          nextKeyP path
          go r'

        else pure r'



-- | Applicative object parser.
data PairsA a = forall x. PairsAp (PairsA (x -> a)) (PairsA x)
              | forall x. PairsMap (x -> a) (PairsA x)
              | PairsOne Text (Qualifier a) (Decoder a)
              | PairsPure a

instance Functor PairsA where
  fmap = PairsMap

instance Apply PairsA where
  (<.>) = PairsAp

instance Applicative PairsA where
  pure = PairsPure

  (<*>) = (<.>)

instance Pair PairsA where
  pair      key one = PairsOne key Mandatory one
  pairMaybe key one = PairsOne key Optional  one



-- | Parse a JSON object as a tuple of key/value pairs.
--
--   Fails if any key in the parser is mentioned multiple times.
--
--   In presence of duplicate keys in the supplied JSON object prefers earlier entries.
pairsA :: PairsA a -> Decoder a
pairsA x =
  Decoder $ \path bits k ->
    case k of
      O -> case packPairsA Radix.empty x of
             Right r -> do
               r' <- parsePairsA path r
               case applyPairsA r' x of
                 Right a  -> pure a
                 Left key ->
                   let msg = showString "Key " $ shows (Strict.unpack key)
                                                   " not found"

                   in err (path, Malformed Benign msg)

             Left key ->
               let msg = showString "Malformed applicative parser: key "
                           . shows (Strict.unpack key)
                           $ " mentioned multiple times"

               in err (path, Malformed Benign msg)

      _ ->
        let !bits' = bits <> ObjectBit
        in err (path, Mismatch bits' k)



packPairsA :: RadixTree State -> PairsA a -> Either Text (RadixTree State)
packPairsA r x =
  case x of
    PairsAp g b        -> do
      r' <- packPairsA r g
      packPairsA r' b

    PairsMap _ b       -> packPairsA r b

    PairsOne key _ dec ->
      case lookupText key r of
        Just _  -> Left key
        Nothing ->
          let !a = Unsaturated $ (unsafeCoerce :: Decoder x -> Decoder Any) dec

              !r' = insertText key a r

          in Right r'

    PairsPure _        -> Right r


applyPairsA :: RadixTree State -> PairsA a -> Either Text a
applyPairsA r = go
  where
    go :: PairsA a -> Either Text a
    go x =
      case x of
        PairsAp g b      -> go g <*> go b

        PairsMap f b     -> fmap f (go b)

        PairsOne key qual (_ :: Decoder a) ->
          let malformed = throw $ MalformedParser
          in case findText malformed key r of
               Saturated a   -> Right $ (unsafeCoerce :: Any -> a) a
               Unsaturated _ ->
                 case qual of
                   Mandatory -> Left key
                   Optional  -> Right Nothing

        PairsPure a    -> Right a
