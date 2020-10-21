{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module AI where

import           Protolude (maximumMay, minimumMay)
import           Data.Tuple.Homogenous
import Mu.Players
import           Data.Function.Syntax
import Control.Concurrent.Async
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Control.Monad.State.Lazy 
import Data.List.NonEmpty (NonEmpty((:|)))
import Control.Applicative (Alternative(..))
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Functor.Compose


minimaxTwo :: Ord score => Int -> Bool -> Tree score -> score
minimaxTwo depth maxPlayer (Branch a deeper) = 
  if depth == 0 
    then a
    else case maxOrMin $ fmap (minimaxTwo (depth - 1) (not maxPlayer)) deeper of
      Nothing -> a
      Just a' -> a'
      where
        maxOrMin =
          if maxPlayer
            then maximumMay
            else minimumMay

data Timeout = Timeout 

newtype Cont a = Cont (IO (a, Maybe (Cont a)))

improveTimeLimit :: Semigroup a => a -> IO Timeout -> Cont a -> IO a
improveTimeLimit init time (Cont value) = do 
  e <- race time value
  case e of
    Left Timeout -> pure init
    Right (a, Nothing) -> pure (init <> a)
    Right (a, Just nextCont) -> improveTimeLimit (init <> a) time nextCont

maximumMayBy :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maximumMayBy f = foldr f' Nothing where
  f' a Nothing = Just a
  f' l (Just r) = case compare (f l) (f r) of
    GT -> Just l
    otherwise -> Just r


minimaxNB
  :: (Ord score, Cycling player) =>
     (player -> scores -> score) -> Int -> player -> Tree scores -> scores
minimaxNB playerScore = minimax' where
  minimax' depth player (Branch scores deeper) = 
    if depth == 0
      then scores
      else case maximumMayBy (playerScore player) $ fmap (minimax' (depth - 1) (successor player)) deeper of
        Nothing -> scores
        Just scores' -> scores'

minimaxN :: (Ord score, Cycling player) => (node -> [node]) -> (player -> node -> score) -> Int -> player -> node -> score
minimaxN children playerScore depth player = playerScore player . minimax' depth player where
  minimax' depth player node = 
    if depth == 0
      then node
      else case maximumMayBy (playerScore player) $ fmap (minimax' (depth -1) (successor player)) (children node) of
        Nothing -> node
        Just n -> n

data Tree a = Branch a [Tree a] 

--improve :: 
--improve choose improveValue (Branch a []) = Branch a

data Explored = Explored | NotExplored

depthFirst update a ((Branch a' deeper) :| wider) = (update a a', deeper ++ wider)

widthFirst update a ((Branch a' deeper) :| wider) = (update a a', wider ++ deeper)

improve merge update a ((Branch a' unsorted) :| sorted) = (update a a', merge unsorted sorted) 

depthFirstSearch p (Branch a deeper) = if p a then Just a else firstJust (depthFirstSearch p) deeper

--depthFirstSearch p (Branch fa deeper) = do
--  a <- fa 
--  if p a then pure (Just a) else depthFirstSearch < 


widthFirstSearch p b = widthFirst [b] where
  widthFirst [] = pure Nothing
  widthFirst ((Branch fa deeper):wider) = do
    a <- fa
    if p a then pure (Just a) else widthFirst (wider ++ deeper)

firstJust f [] = Nothing
firstJust f (x:xs) = f x <|> firstJust f xs

firstJust2 :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstJust2 f as = runMaybeT (firstJust' as) where
  firstJust' [] = MaybeT $ pure Nothing
  firstJust' (a:as) = (MaybeT $ f a) <|> firstJust' as



--minimaxTwoMemoized maxPlayer depth (Branch a rest) = if depth == 0
--  then a
--  else case m



lcs _ [] = []
lcs [] _ = []
lcs (x:xs) (y:ys) 
  | x == y = x:(lcs xs ys)
  | otherwise = longer (lcs xs (y:ys)) (lcs (x:xs) ys) where
    longer xs ys = if length xs > length ys then xs else ys

lcsImproved xs ys = runState (lcs xs ys) Map.empty where
  lcs _ [] = pure []
  lcs [] _ = pure []
  lcs (x:xs) (y:ys) = do
    s <- get
    case Map.lookup (x:|xs, y:|ys) s of
      Just a -> pure a
      Nothing -> record (x:|xs, y:|ys) =<< 
        if x == y 
          then (x:) <$> lcs xs ys 
          else longer <$> lcs (x:xs) ys <*> lcs xs (y:ys)
  record a b = b <$ modify (Map.insert a b) 
  longer xs ys = if length xs > length ys then xs else ys
        
lcs2 xs ys = runState (lcs xs ys) Map.empty where
  lcs [] _ = pure []
  lcs _ [] = pure []
  lcs (x:xs) (y:ys) = memoized (uncurry lcsNE) (x:|xs,y:|ys)
  lcsNE (x:|xs) (y:|ys) 
    | x == y = (x:) <$> lcs xs ys
    | otherwise = longer <$> lcs (x:xs) ys <*> lcs xs (y:ys)
  longer xs ys = if length xs > length ys then xs else ys

memoized :: (Ord a, MonadState (Map a b) m) => (a -> m b) -> a -> m b
memoized f a = do
  map <- get
  case Map.lookup a map of
    Just b -> pure b
    Nothing -> f a >>= record a
  where
    record a b = b <$ modify (Map.insert a b)

