module Main where

import           AuctionCLI
import           AuctionPlay
import           Deal
import           Lib

main :: IO ()
main = someFunc
--fivePlayerAuction :: IO ()
--fivePlayerAuction = do
--  startingHands <- newFivePlayerInitialHands
--  finish <- auctionRound cliInteractions startingHands
--  putStrLn $ show finish
