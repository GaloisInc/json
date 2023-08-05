module Data.RadixTree.Char
  ( RadixTree

  , Data.RadixTree.Char.empty
  , singleton

  , lookup
  , dirtyLookup

  , insert
  , insertUnique

  , View (..)
  , view

  , showsTree
  , toAscList
  ) where

import           Data.Char
import           Data.Functor.Const
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import           Data.Maybe
import           Data.Monoid (Endo (..))
import           Data.Primitive.PrimArray
import           GHC.Base (unsafeChr)
import           Prelude hiding (lookup)



data RadixTree a = Empty
                 | RadixTree (Edge a)
                   deriving Show

data Edge a = Edge (Maybe (PrimArray Char)) (Maybe a) (IntMap (Edge a))
              deriving Show



showsTree :: (a -> ShowS) -> RadixTree a -> ShowS
showsTree showx (RadixTree edge0) = go id edge0
  where
    go prefix (Edge mayPrim mayx deeper) =
      let prefix' = case mayPrim of
                      Nothing   -> prefix
                      Just prim -> prefix . (primArrayToList prim <>)

      in showString (prefix' [])
           . maybe id (\x -> showString " := " . showx x) mayx
           . showChar '\n' . dive prefix' deeper

    dive prefix =
      let apply w = mappend . Endo . go (prefix . (:) (unsafeChr w))
      in appEndo . IntMap.foldrWithKey apply (Endo id)

showsTree _ Empty = showString "<empty>"



empty :: RadixTree a
empty = Empty

singleton :: String -> a -> RadixTree a
singleton str = RadixTree . edge str

{-# INLINE edge #-}
edge :: String -> a -> Edge a
edge str a =
  let mayPrim = case str of
                  c:cs -> Just $ primArrayFromList (c:cs)
                  []   -> Nothing

  in Edge mayPrim (Just a) IntMap.empty



lookup :: String -> RadixTree a -> Maybe a
lookup str0 (RadixTree edge0) = go edge0 str0
  where
    go (Edge mayPrim mayx deeper) str1 =
      case mayPrim of
        Just prim -> goarr 0 str1
          where
            goarr n str =
              case str of
                c:cs -> if c == indexPrimArray prim n
                          then if n + 1 < sizeofPrimArray prim
                                 then goarr (n + 1) cs
                                 else dive cs

                          else Nothing

                [] -> mayx

        Nothing -> dive str1

      where
        {-# INLINE dive #-}
        dive (c:cs) = case IntMap.lookup (ord c) deeper of
                        Just edge' -> go edge' cs
                        Nothing    -> Nothing

        dive    []  = mayx

lookup _ Empty = Nothing



moduleLoc :: ShowS
moduleLoc = (<>) "json.Data.RadixTree.UTF16: "

dirtyLookup :: String -> RadixTree a -> a
dirtyLookup str tree =
  case lookup str tree of
    Just val -> val
    Nothing  ->
      errorWithoutStackTrace . moduleLoc $ "Lookup on key " <> showString str " failed"



insert :: String -> a -> RadixTree a -> RadixTree a
insert str0 a (RadixTree edge0) = RadixTree $ go edge0 str0
  where
    go (Edge mayPrim mayx deeper) str1 =
      case mayPrim of
        Just prim -> goarr 0 str1
          where
            goarr n str =
              let p = indexPrimArray prim n
                  n' = n + 1

                  prev  | n == 0    = Nothing
                        | otherwise = Just $ clonePrimArray prim 0 n

                  prim' | n' == sizeofPrimArray prim = Nothing
                        | otherwise                  =
                            Just $ clonePrimArray prim n' (sizeofPrimArray prim - n')

              in case str of
                   c:cs ->
                     if p == c
                       then if n + 1 < sizeofPrimArray prim
                              then goarr (n + 1) cs
                              else split cs

                       else Edge prev Nothing $
                                IntMap.insert (ord p) (Edge prim' mayx deeper)
                              $ IntMap.singleton (ord c) (edge cs a)

                   [] -> Edge prev (Just a) $
                           IntMap.singleton (ord p) (Edge prim' mayx deeper)

        Nothing -> split str1

      where
        {-# INLINE split #-}
        split (c:cs) = let alt k = Just $ case k of
                                            Just edge' -> go edge' cs
                                            Nothing    -> edge cs a

                       in Edge mayPrim mayx $ IntMap.alter alt (ord c) deeper

        split    []  = Edge mayPrim (Just a) deeper

insert str a Empty = singleton str a



-- | Returns 'Nothing' if the key is already present in the tree.
insertUnique :: String -> a -> RadixTree a -> Maybe (RadixTree a)
insertUnique str0 a (RadixTree edge0) = RadixTree <$> go edge0 str0
  where
    go (Edge mayPrim mayx deeper) str1 =
      case mayPrim of
        Just prim -> goarr 0 str1
          where
            goarr n str =
              let p = indexPrimArray prim n
                  n' = n + 1

                  prev  | n == 0    = Nothing
                        | otherwise = Just $ clonePrimArray prim 0 n

                  prim' | sizeofPrimArray prim - n' == 0 = Nothing
                        | otherwise                      =
                            Just $ clonePrimArray prim n' (sizeofPrimArray prim - n')

              in case str of
                   c:cs ->
                     if p == c
                       then if n + 1 < sizeofPrimArray prim
                              then goarr (n + 1) cs
                              else split cs

                       else Just . Edge prev Nothing $
                                       IntMap.insert (ord p) (Edge prim' mayx deeper)
                                     $ IntMap.singleton (ord c) (edge cs a)

                   [] -> Just . Edge prev (Just a) $
                                  IntMap.singleton (ord p) (Edge prim' mayx deeper)

        Nothing -> split str1

      where
        {-# INLINE split #-}
        split (c:cs) = let alt k = Just <$> case k of
                                              Just edge' -> go edge' cs
                                              Nothing    -> Just $ edge cs a

                       in Edge mayPrim mayx <$> IntMap.alterF alt (ord c) deeper

        split    []  = case mayx of
                         Nothing -> Just $ Edge mayPrim (Just a) deeper
                         Just _  -> Nothing

insertUnique str a Empty = Just $ singleton str a



data Product f g a = Pair (f a) (g a)

instance (Functor f, Functor g) => Functor (Product f g) where
  fmap f (Pair a b) = Pair (fmap f a) (fmap f b)



data View a = View (Char -> View a) (Maybe a) (a -> RadixTree a)
            | Free (String -> a -> RadixTree a)

view :: RadixTree a -> View a
view (RadixTree edge0) = go RadixTree edge0
  where
    go back (Edge mayPrim mayx deeper) =
      case mayPrim of
        Just prim -> goarr 0
          where
            goarr n =
              let p = indexPrimArray prim n
                  n' = n + 1

                  prev  | n == 0    = Nothing
                        | otherwise = Just $ clonePrimArray prim 0 n

                  prim' | n' == sizeofPrimArray prim = Nothing
                        | otherwise                  =
                            Just $ clonePrimArray prim n' (sizeofPrimArray prim - n')
              in View
                   (\c ->
                      if c == p
                        then if n' < sizeofPrimArray prim
                               then goarr n'
                               else dive
                        else Free $ \str a ->
                               back . Edge prev Nothing $
                                          IntMap.insert (ord p) (Edge prim' mayx deeper)
                                        $ IntMap.singleton (ord c) (edge str a)
                   )
                   Nothing
                   (\a -> back . Edge prev (Just a) $
                                   IntMap.singleton (ord p) (Edge prim' mayx deeper)
                   )

        Nothing   -> dive

     where
       dive =
         View
           (\c -> let Pair put (Const get) =
                        IntMap.alterF (Pair Just . Const) (ord c) deeper

                  in case get of
                       Just edge' -> go (back . Edge mayPrim mayx . put) edge'
                       Nothing    -> Free $ \str a ->
                                       back . Edge mayPrim mayx $
                                                put $ edge str a
           )
           mayx
           (\a -> back $ Edge mayPrim (Just a) deeper)
 
view Empty = Free singleton



toAscList :: RadixTree a -> [(String, a)]
toAscList (RadixTree edge0) = go id edge0 []
  where
    go :: (String -> String) -> Edge a -> [(String, a)] -> [(String, a)]
    go prefix (Edge mayPrim mayx deeper) =
      let prefix' = case mayPrim of
                      Nothing   -> prefix
                      Just prim -> prefix . (primArrayToList prim <>)

      in case mayx of
           Nothing -> dive prefix' deeper
           Just x  -> (:) (prefix' [], x) . dive prefix' deeper

    dive :: (String -> String) -> IntMap (Edge a) -> [(String, a)] -> [(String, a)]
    dive prefix =
      let apply w = mappend . Endo . go (prefix . (:) (unsafeChr w))
      in appEndo . IntMap.foldrWithKey apply (Endo id)

toAscList Empty = []
