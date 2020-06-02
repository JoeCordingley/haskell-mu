{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Util where

import           Control.Applicative.Free (Ap (..), liftAp, runAp)
import           Control.Lens             hiding ((<.>))
import           Data.Functor.Identity

data Mono x y a where
  Mono :: x -> Mono x y y

liftMono :: x -> Ap (Mono x y) y
liftMono = liftAp . Mono

unMono :: (x -> y) -> Mono x y a -> a
unMono f (Mono x) = f x

runMono :: (x -> y) -> Ap (Mono x y) a -> a
runMono f = runIdentity . runAp (Identity . unMono f)

insertion :: Ord x => x -> Ap (Mono x y) a -> (x, Ap (Mono x y) a)
insertion x (Pure a) = (x, Pure a)
insertion x (Ap (Mono x') g) =
  if x <= x'
    then (x, Ap (Mono x') g)
    else let (x'', g') = insertion x g
          in (x', Ap (Mono x'') g')

sortAp :: Ord x => Ap (Mono x y) a -> Ap (Mono x y) a
sortAp (Pure a) = Pure a
sortAp (Ap (Mono o) f) =
  let (o', f') = insertion o (sortAp f)
   in Ap (Mono o') f'

sortTraversable :: (Ord x, Traversable t) => t x -> t x
sortTraversable = runMono id . sortAp . traverse liftMono

sortTraversal :: Ord a => Traversal' s a -> s -> s
sortTraversal tr = runMono id . sortAp . tr liftMono

sortOn :: (Ord b, Traversable t) => (a -> b) -> t a -> t a
sortOn f = fmap originalValue . sortTraversable . fmap (sortOnObj f)

data SortOn a b =
  SortOn
    { sortedOn      :: a
    , originalValue :: b
    }

instance Eq a => Eq (SortOn a b) where
  SortOn l _ == SortOn r _ = l == r

instance Ord a => Ord (SortOn a b) where
  SortOn l _ <= SortOn r _ = l <= r

sortOnObj :: (a -> b) -> a -> SortOn b a
sortOnObj f a = SortOn (f a) a

mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd f a = (a, f a)

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
  | x == y = ys
  | otherwise = y : (remove x ys)

minus :: Eq a => [a] -> [a] -> [a]
minus = foldr remove
