{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module New.Exercise where

import Control.Applicative.Free (Ap(..), liftAp, runAp)
import Data.Functor.Identity
import Control.Lens

data Mono x y a where
  Mono :: x -> Mono x y y

liftMono :: x -> Ap (Mono x y) y
liftMono = liftAp . Mono

unMono :: (x -> y) -> Mono x y a -> a
unMono f (Mono x) = f x

runMono :: (x -> y) -> Ap (Mono x y) a -> a
runMono f = runIdentity . runAp (Identity . unMono f)

insertion :: Ord x => x -> Ap (Mono x y) a -> (x, Ap (Mono x y) a)
insertion x (Pure a        ) = (x, Pure a)
insertion x (Ap (Mono x') g) = if x <= x'
  then (x, Ap (Mono x') g)
  else let (x'', g') = insertion x g in (x', Ap (Mono x'') g')

sortAp :: Ord x => Ap (Mono x y) a -> Ap (Mono x y) a
sortAp (Pure a       ) = Pure a
sortAp (Ap (Mono o) f) = let (o', f') = insertion o (sortAp f) in Ap (Mono o') f'

sortTraversable :: (Ord x, Traversable t) => t x -> t x
sortTraversable = runMono id . sortAp . traverse liftMono

sortTraversal :: Ord a => Traversal' s a -> s -> s
sortTraversal tr = runMono id . sortAp . tr liftMono

