module AuctionCLI
  (
  ) where

import           AuctionFunctions
import           Cards
import           Control.Monad.State.Lazy
import           Data.List.Index
import           Data.List.Split
import qualified Data.Map.Lazy            as Map
import           Data.Maybe

type MaxBid = Int

parseCommaSeparatedInts :: String -> Maybe [Int]
parseCommaSeparatedInts s = traverse read $ splitOn "," s

intsToCards :: [Int] -> Map.Map Int Card -> Maybe [Card]
intsToCards [] _ = Just []
intsToCards (i:is) indexed = do
  card <- Map.lookup i indexed
  rest <- intsToCards is $ Map.delete i indexed
  return $ card : rest

cardsToBid :: Int -> [Card] -> Maybe Bid
cardsToBid _ [] = Just Pass
cardsToBid maxBid cards
  | length cards <= maxBid = Just (Raise cards)
  | otherwise = Nothing

parseBid :: Int -> Map.Map Int Card -> String -> Maybe Bid
parseBid maxBid indexed s = do
  ints <- parseCommaSeparatedInts s
  cards <- intsToCards ints indexed
  cardsToBid maxBid cards

tryTryAgain :: IO (Maybe a) -> IO a
tryTryAgain f = do
  maybeA <- f
  case maybeA of
    Just a -> return a
    Nothing -> do
      putStrLn "incorrect input, try again"
      tryTryAgain f

getBidMaybe :: Int -> Map.Map Int Card -> IO (Maybe Bid)
getBidMaybe maxBid indexed = fmap (parseBid maxBid indexed) getLine

getBidIO :: Player -> AuctionState -> IO Bid
getBidIO player state = do
  putStr $ prompt
  tryTryAgain $ getBidMaybe maxBid indexedCards
  where
    cards = fromJust . Map.lookup player $ cardsInHand state
    indexedCards = Map.fromList $ indexed cards
    prompt = bidPromptString player maxBid indexedCards
    maxBid = maximum . Map.elems . bidTotals . cardsBid $ state

bidPromptString :: Player -> MaxBid -> Map.Map Int Card -> String
bidPromptString player maxBid indexed = foldr (++) maxBidString indexStrings
  where
    maxBidString =
      show player ++ ", you can bid a maximum of " ++ show maxBid ++ "\n"
    indexStrings = map indexString $ Map.toList indexed
    indexString pair = show pair ++ "\n"
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
