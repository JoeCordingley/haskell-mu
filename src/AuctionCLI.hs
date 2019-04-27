module AuctionCLI
  ( cliInteractions
  , parseCommaSeparatedInts
  , parseBid
  ) where

import           AuctionFunctions
import           AuctionPlay
import           Cards
import           Control.Monad.State.Lazy
import           Data.List.Index
import           Data.List.Split
import qualified Data.Map.Lazy            as Map
import           Data.Maybe
import           Text.Read

type MaxBid = Int

type Player = Int

parseCommaSeparatedInts :: String -> Maybe [Int]
parseCommaSeparatedInts "" = Just []
parseCommaSeparatedInts s  = traverse readMaybe $ splitOn "," s

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

findOrEmptyList :: (Ord k) => k -> Map.Map k [a] -> [a]
findOrEmptyList = Map.findWithDefault []

getBidCLI :: Int -> Player -> [Card] -> IO Bid
getBidCLI maxBid player cards = do
  putStr $ bidPromptString player maxBid indexedCards
  tryTryAgain $ getBidMaybe maxBid indexedCards
  where
    indexedCards = Map.fromList $ indexed cards

bidPromptString :: Player -> MaxBid -> Map.Map Int Card -> String
bidPromptString player maxBid indexed = maxBidLine ++ indexLines
  where
    maxBidLine =
      show player ++ ", you can bid a maximum of " ++ show maxBid ++ "\n"
    indexLines = showOnePerLine $ Map.toList indexed

showOnePerLine :: Show a => [a] -> String
showOnePerLine as = foldr (++) "" strings
  where
    strings = map (addNewLine . show) as
    addNewLine s = s ++ "\n"

trumpPrompt :: Player -> String
trumpPrompt player = show player ++ ", you can select from these trumps:\n"

getTrumpCLI :: Player -> [Trump] -> IO Trump
getTrumpCLI player = getResponseCLI (trumpPrompt player) player

getResponseCLI :: Show a => String -> Player -> [a] -> IO a
getResponseCLI startingPrompt player as = do
  putStr startingPrompt
  putStr $ showOnePerLine indexedAs
  tryTryAgain . getResponseMaybe $ Map.fromList indexedAs
  where
    indexedAs = indexed as

parseIndexed :: Map.Map Int a -> String -> Maybe a
parseIndexed indexed s = do
  i <- readMaybe s
  Map.lookup i indexed

getResponseMaybe :: Map.Map Int a -> IO (Maybe a)
getResponseMaybe indexed = fmap (parseIndexed indexed) getLine

getPartnerCLI :: Player -> [Player] -> IO Player
getPartnerCLI player = getResponseCLI (partnerPrompt player) player

partnerPrompt :: Player -> String
partnerPrompt player = show player ++ ", you can select from these players:\n"

cliInteractions :: Interactions IO Player
cliInteractions =
  Interactions
    {getBid = getBidCLI, getTrump = getTrumpCLI, getPartner = getPartnerCLI}
