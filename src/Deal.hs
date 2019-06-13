module Deal(deal, Shuffle(..)) where

import           AuctionFunctions
import           Cards
import           Data.List.Split
import qualified Data.Map.Lazy         as Map

type Player = Int

divideEvenly :: Int -> [a] -> [[a]]
divideEvenly n as = chunksOf s as
  where
    s = length as `div` n

class Functor m => Shuffle m where
  shuffle :: [a] -> m [a]

deal :: Shuffle m => [p] -> [a] -> m [(p,[a])]
deal players cards = zip players <$> dividedCards where
  dividedCards = divideEvenly numberOfPlayers <$> shuffle cards
  numberOfPlayers = length cards

