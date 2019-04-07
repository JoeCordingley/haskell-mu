module AuctionCLI
  (
  ) where

import           AuctionFunctions
import           AuctionPlay
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

findOrEmptyList :: (Ord k) => k -> Map.Map k [a] -> [a]
findOrEmptyList = Map.findWithDefault []

--getBidIO :: Player -> AuctionState -> IO Bid
--getBidIO player state = getResponseIO prompt indexedCards where
--  indexedCards = Map.fromList . indexed . findOrEmptyList player $ cardsInHand state
--  prompt = bidPromptString player maxBid indexedCards
--  maxBid = maximum . Map.elems . bidTotals . cardsBid $ state

getBidIO :: Player -> AuctionState -> IO Bid
getBidIO player state = do
  putStr $ prompt
  tryTryAgain $ getBidMaybe maxBid indexedCards
  where
    cards = fromJust . Map.lookup player $ cardsInHand state
    indexedCards = Map.fromList $ indexed cards
    prompt = bidPromptString player maxBid indexedCards
    maxBid = maximum . Map.elems . bidTotals . cardsBid $ state

getResponseIO :: String -> Map.Map Int a -> IO a
getResponseIO prompt indexed = do
  putStr $ prompt
  tryTryAgain $ getResponseMaybe indexed

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

trumpPromptString :: Player -> Map.Map Int Trump -> String
trumpPromptString player indexed = firstLine ++ otherLines
  where
    firstLine = show player ++ ", you can select from these trumps: \n"
    otherLines = showOnePerLine $ Map.toList indexed

getTrumpIO :: Player -> AuctionState -> IO Trump
getTrumpIO player state = getResponseIO prompt indexedTrumps where
  cards = findOrEmptyList player $ cardsBid state
  indexedTrumps = Map.fromList . indexed . availableTrumps $ cards
  prompt = trumpPromptString player indexedTrumps

--getTrumpIO :: Player -> AuctionState -> IO Trump
--getTrumpIO player state = do
--  putStr $ prompt
--  tryTryAgain $ getResponseMaybe indexedTrumps
--  where
--    cards = fromJust . Map.lookup player $ cardsBid state
--    indexedTrumps = Map.fromList . indexed . availableTrumps $ cards
--    prompt = trumpPromptString player indexedTrumps

parseIndexed :: Map.Map Int a -> String -> Maybe a
parseIndexed indexed s = do
  i <- read s
  Map.lookup i indexed

getResponseMaybe :: Map.Map Int a -> IO (Maybe a)
getResponseMaybe indexed = fmap (parseIndexed indexed) getLine


getPartnerIO :: Player -> Map.Map Int Player -> IO Player
getPartnerIO player availablePlayers = getResponseIO prompt availablePlayers where
  prompt = partnerPromptString player availablePlayers

partnerPromptString :: Player -> Map.Map Int Player -> String
partnerPromptString player indexed = firstLine ++ otherLines
  where
    firstLine = show player ++ ", you "
    otherLines = showOnePerLine $ Map.toList indexed

parsePartner :: Map.Map Int Player -> String -> Maybe Player
parsePartner indexed s = do
  i <- read s
  Map.lookup i indexed

getPartnerMaybe :: Map.Map Int Player -> IO (Maybe Player)
getPartnerMaybe indexed = fmap (parsePartner indexed) getLine

--getBidAy player = StateT (\s -> fmap (\bid -> (s,bid)) getBidIO player s)
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
