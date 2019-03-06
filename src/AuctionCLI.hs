module AuctionCLI() where

import AuctionFunctions
import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.List.Index
import Data.List.Split
import Cards

type MaxBid = Int

bidPromptString :: Player -> MaxBid -> Map.Map Int Card -> String
bidPromptString player maxBid indexed = foldr (++) maxBidString indexStrings where 
  maxBidString = show player ++ ", you can bid a maximum of " ++ show maxBid ++ "\n"
  indexStrings = map indexString $ Map.toList indexed
  indexString pair = show pair ++ "\n"

parseCommaSeparatedInts :: String -> Maybe [Int]
parseCommaSeparatedInts s = traverse read $ splitOn "," s

validateBid :: [Int] -> Map.Map Int Card -> Maybe Bid
validateBid ints = fmap toBid . maybeCards ints where
  toBid [] = Pass
  toBid cards = Raise cards
  maybeCards [] _ = Just []
  maybeCards (i:is) indexed = do
    card <- Map.lookup i indexed
    rest <- maybeCards is $ Map.delete i indexed
    return $ card:rest

parseBid ::  Map.Map Int Card -> String -> Maybe Bid
parseBid indexed s = do 
  ints <- parseCommaSeparatedInts s
  validateBid ints indexed


tryTryAgain :: IO (Maybe a) -> IO a
tryTryAgain f = do
  maybeA <- f
  case maybeA of 
    Just a -> return a
    Nothing -> do
      putStrLn "incorrect input, try again"
      tryTryAgain f

getBidMaybe :: Map.Map Int Card -> IO (Maybe Bid) 
getBidMaybe indexed = fmap (parseBid indexed) getLine

getBidIO :: Player -> AuctionState -> IO Bid
getBidIO player state = do
  putStr $ prompt
  tryTryAgain $ getBidMaybe indexedCards where
    cards = fromJust . Map.lookup player $ cardsInHand state
    indexedCards = Map.fromList $ indexed cards
    prompt = undefined
    


--parseBidString :: [(Int,Card)] ->  String -> Bid 
--parseBidString indexed s = if 
--bidPrompt :: MaxBid -> [(Int,Card)] -> IO ()
--bidPrompt max indexed = do
--  _ <- putStrLn $ "you can bid a maximum of " ++  show max
--  _ <- traverse (putStrLn . show) indexed
--  return ()

--getBid :: Player -> StateT AuctionState IO Bid
--getBid player = StateT getBidIO
--  where
--    getBidIO state = do
--      _ <- putStrLn . bidPrompt () indexedCards state
--      undefined
--    cards = fromJust . Map.lookup player . cardsInHand
--    indexedCards = indexed . cards
