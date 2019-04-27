module Deal where

import           AuctionFunctions
import           Cards
import           Data.List.Split
import qualified Data.Map.Lazy         as Map
import           System.Random.Shuffle

type Player = Int

shuffleAndDivide :: Ord a => [a] -> [b] -> IO (Map.Map a [b])
shuffleAndDivide as =
  fmap (Map.fromList . zip as . divideEvenly (length as)) . shuffleM

divideEvenly :: Int -> [a] -> [[a]]
divideEvenly n as = chunksOf s as
  where
    s = length as `div` n

newFivePlayerAuctionState :: IO (AuctionState Player)
newFivePlayerAuctionState =
  fmap initialState $ shuffleAndDivide [1 .. 5] fullDeck


newFivePlayerInitialHands :: IO (Map.Map Player [Card])
newFivePlayerInitialHands = shuffleAndDivide [1 .. 5] fullDeck
