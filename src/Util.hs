module Util
  ( findOrEmptyList
  , minus
  , remove
  ) where

import qualified Data.Map.Lazy as Map

findOrEmptyList :: (Ord k) => k -> Map.Map k [a] -> [a]
findOrEmptyList = Map.findWithDefault []

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
  | x == y = ys
  | otherwise = y : (remove x ys)

minus :: Eq a => [a] -> [a] -> [a]
minus xs ys = foldl (flip remove) xs ys
