{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module BiddingSpec
  ( biddingProperties
  ) where

import           Bidding
import           Cards
import           Control.Monad.Writer.Lazy
import           Data.List
import           Data.Maybe
import           Deal
import           Test.Tasty
import           Test.Tasty.QuickCheck     hiding (shuffle)
import qualified Test.Tasty.QuickCheck     as QC (shuffle)
import           Util
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy (Map)
import           Control.Monad.State.Lazy

biddingProperties :: TestTree
biddingProperties = testGroup "Bidding" [playersAskedInSequence, playEndsAfterFullSequenceOfPasses, playersGivenCorrectMaximum, playersRequestFromCardsLeft, biddingReturnsRaisesInOrder, biddingReturnsTheCardsLeftInHand ]

playersAskedInSequence :: TestTree
playersAskedInSequence =
  testProperty "players asked in turn starting from first player" $
  forAll biddingPlayers playersAskedInSequence'
  where
    playersAskedInSequence' (initialPlayerSequence, playersAskedInTurn) =
      playersAskedInTurn `isPrefixOf` (cycle initialPlayerSequence)

playEndsAfterFullSequenceOfPasses :: TestTree
playEndsAfterFullSequenceOfPasses = testProperty "play ends after first full sequence of passes" $ forAll biddingBids playEndsAfterFullSequenceOfPasses' where
  playEndsAfterFullSequenceOfPasses' (numberOfPlayers, bids) = indexAtLastPass == Just(length bids) where 
    indexAtLastPass = indexAtLastPass' 1 0 bids 
    indexAtLastPass' _ _ [] = Nothing
    indexAtLastPass' index _ (Raise _ : rest) = indexAtLastPass' (index+1) 0 rest
    indexAtLastPass' index passes (Pass: rest) = if passes +1 == numberOfPlayers then Just index else indexAtLastPass' (index + 1) (passes + 1) rest

playersGivenCorrectMaximum :: TestTree
playersGivenCorrectMaximum = testProperty "players given a maximum that would take them one ahead of current max" $ forAll maxBidGen allMaxesAreCorrect where
  allMaxesAreCorrect = all maxIsCorrect
  maxIsCorrect (maxBid, topTotal, playerTotal) = playerTotal + maxBid == topTotal + 1

playersRequestFromCardsLeft :: TestTree
playersRequestFromCardsLeft = testProperty "players asked to choose from the cards left in their hand" $ forAll cardsLeftGen cardRequestsAllCorrect where
  cardRequestsAllCorrect (initialHands, requests) = all (requestIsCorrect initialHands) requests
  requestIsCorrect initialHands (player, cardsRequested, cardsBidSoFar) = cardsRequested `containsTheSameElementsAs` (findOrEmptyList player initialHands `minus` cardsBidSoFar)

biddingReturnsRaisesInOrder :: TestTree
biddingReturnsRaisesInOrder = testProperty "bidding returns the player raises in the order they occurred" $ forAll returnedRaisesGen raisesRecorded where
  raisesRecorded (raises, playerBids) = raisesOf playerBids == raises

raisesOf :: [(Player, Bid)] -> [(Player, [Card])]
raisesOf = catMaybes . map playerRaise where
  playerRaise (player , bid) = (\raise -> (player, raise)) <$> raise bid
  raise Pass = Nothing
  raise (Raise cards) = Just cards

biddingReturnsTheCardsLeftInHand :: TestTree
biddingReturnsTheCardsLeftInHand = testProperty "bidding returns the cards left in hand for each player" $ forAll returnedRaisesAndInitialHands cardsInHandIsCorrect where
  cardsInHandIsCorrect (initialHands, cardsLeftInHand, playerBids) = (initialHands `minusMap` playerBids) `containsSameElementsAndKeysAs` cardsLeftInHand

minusMap :: (Ord k, Eq v) => Map k [v] -> Map k [v] -> Map k [v]
minusMap = Map.differenceWith minusStrict

minusStrict :: Eq v =>  [v] -> [v] -> Maybe [v]
minusStrict xs [] = Just xs
minusStrict [] _ = Nothing
minusStrict xs (y:ys) = do
  xs' <- removeStrict y xs
  minusStrict xs' ys

removeStrict :: Eq v => v -> [v] -> Maybe [v]
removeStrict v [] = Nothing
removeStrict v (x:xs) 
  | v == x = Just xs 
  | otherwise = (x:) <$> removeStrict v xs

containsSameElementsAndKeysAs :: (Ord k, Ord v) => Map k [v] -> Map k [v] -> Bool
containsSameElementsAndKeysAs m n = sortedAndSorted m == sortedAndSorted n where
  sortedAndSorted = sort . Map.toList . Map.map sort

containsTheSameElementsAs :: Ord a =>  [a] -> [a] -> Bool
containsTheSameElementsAs xs ys = sort xs == sort ys

type CardsLeftInHand = Map Player [Card]
type CardsBid = Map Player [Card]
type InitialHandsMap = Map Player [Card]




type CardsBidSoFar = [Card]
type CardsRequested = [Card]


type Player = Int

instance Shuffle Gen where
  shuffle = QC.shuffle

startingGen :: Gen (InitialHands Player)
startingGen = do
  numberOfPlayers <- (choose (3, 6) :: Gen Int)
  let players = [1 .. numberOfPlayers]
  dealCards players

data Record = Record
  { recordedMax    :: Int
  , recordedPlayer :: Player
  , recordedCards  :: [Card]
  , recordedBid    :: Bid
  }

type TestContext = WriterT [Record] Gen

recordedBidGen :: MaxBid -> Player -> [Card] -> TestContext Bid
recordedBidGen maxBid player cards = do
  bid <- lift $ bidGen maxBid player cards
  tell
    [ Record
        { recordedMax = maxBid
        , recordedPlayer = player
        , recordedCards = cards
        , recordedBid = bid
        }
    ]
  return bid

--finishedBidding :: TestContext (FinishedBidding Player)
--finishedBidding = do
--  initialHands <- lift startingGen
--  runBidding recordedBidGen initialHands
--
--finishedBiddingPlayers :: Gen (FinishedBidding Player, [Player])
--finishedBiddingPlayers =
--  runWriterT $ mapWritten (fmap recordedPlayer) finishedBidding

type InitialPlayerSequence = [Player]

type PlayersAskedInTurn = [Player]

biddingPlayers :: Gen (InitialPlayerSequence, PlayersAskedInTurn)
biddingPlayers = do
  initialHands <- startingGen
  playersAskedInTurn <-
    execWriterT . mapWritten (fmap recordedPlayer) $
    runBidding recordedBidGen initialHands
  return (map fst initialHands, playersAskedInTurn)

type NumberOfPlayers = Int

biddingBids :: Gen (NumberOfPlayers, [Bid])
biddingBids = do
  initialHands <- startingGen
  bids <- 
    execWriterT . mapWritten (fmap recordedBid) $ 
    runBidding recordedBidGen initialHands
  return (length initialHands, bids)



biddingRecords :: Gen [Record]
biddingRecords = startingGen >>= execWriterT . runBidding recordedBidGen

returnedRaisesGen :: Gen ([(Player, [Card])], [(Player, Bid)])
returnedRaisesGen = startingGen >>= runWriterT . fmap finishedPlayerRaises  . mapWritten (map playerBid) . runBidding recordedBidGen where
  playerBid record = (recordedPlayer record, recordedBid record)

type TopTotal = Int
type PlayerTotal = Int

maxBidGen :: Gen [(MaxBid, TopTotal, PlayerTotal)] 
maxBidGen = totals <$> biddingRecords where
  totals records = zip3 (recordedMax <$> records) (topTotal records) (playerTotal records)
  topTotal = map (maximum . (0:) . Map.elems) . runningTotals . (map playerBid)
  playerTotal = map (uncurry $ Map.findWithDefault 0) . playerAndRunningTotals
  playerAndRunningTotals records = zip (recordedPlayer <$> records) (runningTotals $ map playerBid records)
  playerBid r = (recordedPlayer r, recordedBid r)

runningTotalBids :: [(Player,Bid)] -> [Map Player [Card]]
runningTotalBids = scanl (flip accumulate) Map.empty where
  accumulate (player, Pass) = id
  accumulate (player, Raise cards) = Map.insertWith (++) player cards

runningTotals :: [(Player,Bid)] -> [Map Player Int]
runningTotals = map (Map.map length) . runningTotalBids

cardsLeftGen :: Gen (Map Player [Card], [(Player, CardsRequested, CardsBidSoFar)])
cardsLeftGen = do
  initialHands <- startingGen
  requests <- execWriterT . mapWritten toRequests $ runBidding recordedBidGen initialHands
  return (Map.fromList initialHands, requests)
  where
    toRequests records = zip3 (recordedPlayer <$> records) (recordedCards <$> records) (playerTotal records)
    playerTotal = map (uncurry $ findOrEmptyList) . playerAndRunningTotals
    playerAndRunningTotals records = zip (recordedPlayer <$> records) (runningTotalBids $ map playerBid records)
    playerBid r = (recordedPlayer r, recordedBid r)

returnedRaisesAndInitialHands :: Gen (InitialHandsMap, CardsLeftInHand, CardsBid)
returnedRaisesAndInitialHands = do
  initialHands <- startingGen
  (cardsLeftInHand, cardsBid) <- runWriterT . fmap finishedCardsInHand . mapWritten playerBids $ runBidding recordedBidGen initialHands
  return (Map.fromList initialHands, cardsLeftInHand, cardsBid) 
  where
    playerBids = foldr (uncurry $ Map.insertWith (++)) Map.empty . map playerBid
    playerBid record = (recordedPlayer record, cardsOf $ recordedBid record)
    cardsOf Pass = []
    cardsOf (Raise cards) = cards

mapWritten :: Functor m => (w -> w') -> WriterT w m a -> WriterT w' m a
mapWritten = mapWriterT . fmap . second
  where
    second f (a, b) = (a, f b)

bidGen :: MaxBid -> player -> [Card] -> Gen Bid
bidGen maxBid player cards = do
  numberOfCards <- choose (0, maxBid)
  bidFromCards <$> sublistOfN numberOfCards cards

bidFromCards :: [Card] -> Bid
bidFromCards [] = Pass
bidFromCards cs = Raise cs

sublistOfN :: Eq a => Int -> [a] -> Gen [a]
sublistOfN 0 _ = return []
sublistOfN _ [] = return []
sublistOfN n as = do
  a <- elements as
  let remaining = remove a as
  rest <- sublistOfN (n - 1) remaining
  return $ a : rest

class Monad f => GetBid f player where
  getBid :: Int -> player -> [Card] -> f Bid

newtype RecordWriterT a = RecordWriterT ( WriterT [Record] Gen a) deriving (Monad, Applicative, Functor)

instance GetBid RecordWriterT Int where
  getBid max player cards = do 
    bid <- RecordWriterT . lift $ bidGen max player cards
    RecordWriterT $ tell [ Record { recordedMax = max , recordedPlayer = player , recordedBid = bid , recordedCards = cards } ]
    return bid
