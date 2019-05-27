module Deal where

import           AuctionFunctions
import           Cards
import           Data.List.Split
import qualified Data.Map.Lazy         as Map
import           System.Random.Shuffle

type Player = Int

shuffleAndDivide :: Ord a => [a] -> [b] -> IO  [( a, [b] )]
shuffleAndDivide as =
  fmap (zip as . divideEvenly (length as)) . shuffleM

divideEvenly :: Int -> [a] -> [[a]]
divideEvenly n as = chunksOf s as
  where
    s = length as `div` n

newFivePlayerAuctionState :: IO (AuctionState Player)
newFivePlayerAuctionState =
  fmap initialState $ shuffleAndDivide [1 .. 5] fullDeck

newFivePlayerInitialHands :: IO [(Player, [Card])]
newFivePlayerInitialHands = shuffleAndDivide [1 .. 5] fullDeck
