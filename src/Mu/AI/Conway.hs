{-# LANGUAGE TupleSections  #-}

module Mu.AI.Conway where

import Data.Maybe

data Cell = Alive | Dead deriving (Show, Eq)

tick :: [[Cell]] -> [[Cell]]
tick = (fmap . fmap) (tickOne . countNeighbours) . threeByThrees where
  tickOne (Alive, 2) = Alive
  tickOne (Alive, 3) = Alive
  tickOne (Dead, 3) = Alive
  tickOne _ = Dead
  countNeighbours ((nw,n,ne), (w,c,e),(sw,s,se)) = (c, count (==Alive) $ catMaybes [nw,n,ne,w,e,sw,s,se]) 


threeByThrees :: [[a]] -> [[((Maybe a, Maybe a, Maybe a), (Maybe a, a, Maybe a), (Maybe a, Maybe a, Maybe a))]]
threeByThrees = fmap zipRows . threes . fmap threes where
  zipRows (Nothing, middle, Nothing) = fmap (nothings,,nothings) middle
  zipRows (Nothing, middle, Just bottom) = let join c (sw, s, se) = (nothings,c,(sw, Just s, se)) in zipWith join middle bottom
  zipRows (Just top, middle, Nothing) = let join (nw, n, ne) c = ((nw, Just n, ne), c, nothings) in zipWith join top middle
  zipRows (Just top, middle, Just bottom) = let join (nw, n, ne) (c, (sw, s, se)) = ((nw, Just n, ne), c, (sw, Just s, se)) in zipWith join top $ zip middle bottom
  nothings = (Nothing, Nothing, Nothing)

threes :: [a] -> [(Maybe a, a, Maybe a)]
threes as = zipWith tuple3 (Nothing : fmap Just as) . zip as $ tail (fmap Just as ++ [Nothing]) where
  tuple3 a (b,c) = (a,b,c)


count :: Foldable f => (a -> Bool) -> f a -> Int
count p = foldr (\a -> if p a then (1+) else id) 0
 
