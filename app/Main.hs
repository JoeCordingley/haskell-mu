module Main where

import           Mu.Deal
import qualified Mu.WebsocketTry as WS

main :: IO ()
main = WS.main
--fivePlayerAuction :: IO ()
--fivePlayerAuction = do
--  startingHands <- newFivePlayerInitialHands
--  finish <- auctionRound cliInteractions startingHands
--  putStrLn $ show finish
