module BiddingSpec
  ( biddingProperties
  ) where

import           Bidding
import           Cards
import           Control.Monad.Writer.Lazy
import           Data.List
import           Deal
import           Test.Tasty
import           Test.Tasty.QuickCheck     hiding (shuffle)
import qualified Test.Tasty.QuickCheck     as QC (shuffle)
import           Util
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy (Map)
import           Control.Monad.State.Lazy

biddingProperties :: TestTree
biddingProperties = testGroup "Bidding" [playersAskedInSequence, playEndsAfterFullSequenceOfPasses, playersGivenCorrectMaximum]

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

playersAskedInSequence :: TestTree
playersAskedInSequence =
  testProperty "players asked in turn starting from first player" $
  forAll biddingPlayers playersAskedInSequence'
  where
    playersAskedInSequence' (initialPlayerSequence, playersAskedInTurn) =
      playersAskedInTurn `isPrefixOf` (cycle initialPlayerSequence)
type NumberOfPlayers = Int

biddingBids :: Gen (NumberOfPlayers, [Bid])
biddingBids = do
  initialHands <- startingGen
  bids <- 
    execWriterT . mapWritten (fmap recordedBid) $ 
    runBidding recordedBidGen initialHands
  return (length initialHands, bids)


playEndsAfterFullSequenceOfPasses :: TestTree
playEndsAfterFullSequenceOfPasses = testProperty "play ends after first full sequence of passes" $ forAll biddingBids playEndsAfterFullSequenceOfPasses' where
  playEndsAfterFullSequenceOfPasses' (numberOfPlayers, bids) = indexAtLastPass == Just(length bids) where 
    indexAtLastPass = indexAtLastPass' 1 0 bids 
    indexAtLastPass' _ _ [] = Nothing
    indexAtLastPass' index _ (Raise _ : rest) = indexAtLastPass' (index+1) 0 rest
    indexAtLastPass' index passes (Pass: rest) = if passes +1 == numberOfPlayers then Just index else indexAtLastPass' (index + 1) (passes + 1) rest

biddingRecords :: Gen [Record]
biddingRecords = startingGen >>= execWriterT . runBidding recordedBidGen

type TopTotal = Int
type PlayerTotal = Int

maxBidGen :: Gen [(MaxBid, TopTotal, PlayerTotal)] 
maxBidGen = totals <$> biddingRecords where
  totals records = zip3 (recordedMax <$> records) (topTotal records) (playerTotal records)
  topTotal = map (maximum . (0:) . Map.elems) . runningTotals . (map playerBid)
  playerTotal = map (uncurry $ Map.findWithDefault 0) . playerAndRunningTotals
  playerAndRunningTotals records = zip (recordedPlayer <$> records) (runningTotals $ map playerBid records)
  playerBid r = (recordedPlayer r, recordedBid r)

playersGivenCorrectMaximum :: TestTree
playersGivenCorrectMaximum = testProperty "players given a maximum that would take them one ahead of current max" $ forAll maxBidGen allMaxesAreCorrect where
  allMaxesAreCorrect = all maxIsCorrect
  maxIsCorrect (maxBid, topTotal, playerTotal) = playerTotal + maxBid == topTotal + 1

runningTotals :: [(Player,Bid)] -> [Map Player Int]
runningTotals = scanl (flip accumulate) Map.empty where
  accumulate (player,Pass) = id
  accumulate (player, Raise cards) = Map.insertWith (+) player (length cards)

mapWritten :: Functor m => (w -> w') -> WriterT w m a -> WriterT w' m a
mapWritten = mapWriterT . fmap . second
  where
    second f (a, b) = (a, f b)

bidGen :: MaxBid -> Player -> [Card] -> Gen Bid
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
