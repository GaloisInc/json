{-# LANGUAGE BangPatterns
           , GADTs
           , ScopedTypeVariables
           , TemplateHaskellQuotes
           , UnboxedTuples #-}

module Codec.JSON.Decoder.Object.TH
  ( PairsQ
  , pairsQ
  ) where

import           Codec.JSON.Decoder.Internal
import           Codec.JSON.Decoder.Internal.TH
import           Codec.JSON.Decoder.Object.Internal

import           Control.Exception
import           Data.RadixTree.Word8.Strict (RadixTree)
import qualified Data.RadixTree.Word8.Strict as Radix
import qualified Data.RadixTree.Word8.Strict.Pointer as Radix
import qualified Data.RadixTree.Word8.Strict.TH as Radix
import           Data.Text (Text)
import qualified Data.Text as Strict (unpack)
import           GHC.Exts (Any)
import           Language.Haskell.TH.Syntax
import           Parser.Lathe
import           Unsafe.Coerce



-- | Precompiled applicative object parser.
data PairsQ a = forall x. PairsAp (PairsQ (x -> a)) (PairsQ x)
              | forall x. PairsMap (Code Q (x -> a)) (PairsQ x)
              | PairsOne Text (Qualifier a) (Code Q (Decoder a))
              | PairsPure (Code Q a)

instance FunctorQ PairsQ where
  fmapQ = PairsMap

instance ApplyQ PairsQ where
  (|*|) = PairsAp

instance ApplicativeQ PairsQ where
  pureQ = PairsPure

instance PairQ PairsQ where
  pairQ      key one = PairsOne key Mandatory one
  pairMaybeQ key one = PairsOne key Optional  one



-- | Parse a JSON object as a tuple of key/value pairs.
--
--   In presence of duplicate keys in the supplied JSON object prefers earlier entries.
pairsQ :: PairsQ a -> Code Q (Decoder a)
pairsQ x =
  let proto = packPairsQ Radix.empty x

  in [|| Decoder $ \path bits k ->
           case k of
             O -> do
               let r = $$(joinCode $ Radix.sequenceCode <$> proto)
               r' <- parsePairsA path r
               case $$(joinCode $ do p <- proto; pure $ applyPairsQ p x) r' of
                 Right a  -> pure a
                 Left key ->
                   let msg = showString "Key " $ shows (Strict.unpack key)
                                                   " not found"

                   in err (path, Malformed Benign msg)

             _ ->
               let !bits' = bits <> ObjectBit
               in err (path, Mismatch bits' k)
      ||]



packPairsQ :: RadixTree (Code Q State) -> PairsQ a -> Q (RadixTree (Code Q State))
packPairsQ r x =
  case x of
    PairsAp g b        -> do
      r' <- packPairsQ r g
      packPairsQ r' b

    PairsMap _ b       -> packPairsQ r b

    PairsOne key _ dec ->
      case lookupText key r of
        Just _  -> fail $ showString "Malformed applicative parser: key "
                            $ shows (Strict.unpack key) " is mentioned multiple times"
        Nothing ->
          let !a = [|| Unsaturated $ (unsafeCoerce :: Decoder x -> Decoder Any) $$(dec) ||]

              !r' = insertText key a r

          in pure r'

    PairsPure _        -> pure r



applyPairsQ :: RadixTree any -> PairsQ a -> Code Q (RadixTree State -> Either Text a)
applyPairsQ proto = go
  where
    go :: PairsQ a -> Code Q (RadixTree State -> Either Text a)
    go x =
      case x of
        PairsAp g b -> [|| \r -> $$(go g) r <*> $$(go b) r ||]

        PairsMap f b -> [|| \r -> $$(f) <$> $$(go b) r ||]

        PairsOne key qual (_ :: Code Q (Decoder a)) ->
          [|| \r ->
                let malformed = throw $ MalformedParser
                in case Radix.follow
                          malformed
                          $$( case Radix.pointer (Radix.feedText key) proto of
                                Nothing  -> joinCode $ fail "A pointer fell through"
                                Just ptr -> [|| ptr ||]
                            )
                          r of
                                      -- Awkward coercion: no @a@ at this level.
                     Saturated a   -> Right $ unsafeCoerce a
                     Unsaturated _ ->
                       $$( case qual of
                             Mandatory -> [|| Left key ||]
                             Optional  -> [|| Right Nothing ||]
                         )
           ||]

        PairsPure a    -> [|| \_ -> Right $$(a) ||]
