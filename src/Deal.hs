module Deal where

import qualified Data.Map.Lazy as Map
import Data.List.Split
import System.Random.Shuffle
import AuctionFunctions
import Cards

shuffleAndDivide :: Ord a =>  [a] -> [b] -> IO (Map.Map a [b])
shuffleAndDivide as = fmap (Map.fromList . zip as . divideEvenly (length as )) . shuffleM 

divideEvenly :: Int -> [a] -> [[a]]
divideEvenly n as = chunksOf s as where s = length as `div` n

newFivePlayerAuctionState :: IO AuctionState
newFivePlayerAuctionState = fmap initialState $ shuffleAndDivide [1..5] fullDeck

newFivePlayerInitialHands :: IO (Map.Map Player [Card])
newFivePlayerInitialHands = shuffleAndDivide [1..5] fullDeck

