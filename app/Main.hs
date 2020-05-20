module Main where

import           AuctionCLI
import           AuctionPlay
import           Deal
import           Lib
import qualified Websockets.Websockets as WS

main :: IO ()
main = WS.main
--fivePlayerAuction :: IO ()
--fivePlayerAuction = do
--  startingHands <- newFivePlayerInitialHands
--  finish <- auctionRound cliInteractions startingHands
--  putStrLn $ show finish
