module Util
  ( findOrEmptyList
  , minus
  , remove
  , (.:)
  ) where

import qualified Data.Map.Lazy as Map

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

findOrEmptyList :: (Ord k) => k -> Map.Map k [a] -> [a]
findOrEmptyList = Map.findWithDefault []

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
  | x == y = ys
  | otherwise = y : (remove x ys)

minus :: Eq a => [a] -> [a] -> [a]
minus = foldr remove

fmap' :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r ) -> r
fmap' fa f = \k -> fa $ \a -> f a k

