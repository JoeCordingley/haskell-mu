module Deal
  ( dealCards
  , Shuffle(..)
  ) where

import           AuctionFunctions
import           Cards
import           Data.List.Split
import qualified Data.Map.Lazy    as Map
import Control.Monad.Reader

--import           System.Random.Shuffle
--shuffleAndDivide :: Ord a => [a] -> [b] -> IO  [( a, [b] )]
--shuffleAndDivide as =
--  fmap (zip as . divideEvenly (length as)) . shuffleM
divideEvenly :: Int -> [a] -> [[a]]
divideEvenly n as = chunksOf s as
  where
    s = length as `div` n

--newFivePlayerAuctionState :: IO (AuctionState Player)
--newFivePlayerAuctionState =
--  fmap initialState $ shuffleAndDivide [1 .. 5] fullDeck
--
--newFivePlayerInitialHands :: IO [(Player, [Card])]
--newFivePlayerInitialHands = shuffleAndDivide [1 .. 5] fullDeck
deal :: Shuffle m => [p] -> [a] -> m [(p, [a])]
deal players cards = zip players <$> dividedCards
  where
    dividedCards = divideEvenly numberOfPlayers <$> shuffle cards
    numberOfPlayers = length cards

class Functor m =>
      Shuffle m
  where
  shuffle :: [a] -> m [a]

dealCards :: (Functor m, Shuffle m) => [player] -> m [(player, [Card])]
dealCards players = deal players deck
  where
    deck =
      if length players == 3
        then reducedDeck
        else fullDeck

type Shuffle2 f a = [a] -> f [a]

--shuffleAndDeal :: (Functor f) => [player] -> ReaderT (Shuffle2 f a) f [(player, [Card])]
--shuffleAndDeal players = 


