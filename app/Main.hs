module Main where

import           Lib
import Deal
import AuctionPlay
import AuctionCLI

main :: IO ()
main = someFunc

fivePlayerAuction :: IO ()
fivePlayerAuction = do
  startingHands <- newFivePlayerInitialHands
  finish <- auctionRound cliInteractions startingHands
  putStrLn $ show finish


