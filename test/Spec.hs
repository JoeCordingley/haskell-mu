import           AuctionCLI
import           AuctionFunctions
import           AuctionPlaySpec
import           BiddingSpec
import           Cards
import           Data.List
import qualified Data.Map.Lazy    as Map
import           Test.Tasty
import           Test.Tasty.HUnit

type Player = Int

main :: IO ()
main = do
  defaultMain $
    testGroup
      "tests"
      [sixtyCards, auctionTests, auctionPlayTests, biddingProperties]

auctionTests :: TestTree
auctionTests =
  testGroup
    "auction tests"
    [ eklatNoPoints
    , eklatCase
    , unfinished
    --, chiefAndVice
    --, chiefOnly
    , parseBidWorks
    ]

sixtyCards :: TestTree
sixtyCards = testCase "there should be 60 cards" (length fullDeck @?= 60)

playerOne = 1

playerTwo = 2

playerThree = 3

fivePlayers = [1 .. 5]

fivePasses = replicate 5 Pass

fourPasses = replicate 4 Pass

emptyState = initialState []

playerBidsToStatus :: [Player] -> [Bid] -> AuctionStatus Player
playerBidsToStatus players bids =
  auctionStatus numberOfPlayers $
  foldl
    (\s -> \(player, bid) -> auctionState player bid s)
    emptyState
    playerBids
  where
    numberOfPlayers = length players
    playerBids = cycle players `zip` bids

eklatNoPoints :: TestTree
eklatNoPoints =
  testCase "five passes should be eklat no points" $
  playerBidsToStatus fivePlayers fivePasses @?=
  (Finished $ NoResult EklatNoPoints)

indexedCards =
  Map.fromList [(0, redSeven), (1, greenSeven), (2, blackOne), (3, blueFive)]

parseBidWorks :: TestTree
parseBidWorks =
  testGroup
    "parseBid should return the bid"
    [ testCase "a blank string returns a pass" $
      parseBid 1 Map.empty "" @?= Just Pass
    , testCase "a single number returns a bid with that card" $
      parseBid 1 indexedCards "1" @?= Just (Raise [greenSeven])
    ]

unfinished :: TestTree
unfinished =
  testCase "four passes should be unfinished" $
  playerBidsToStatus fivePlayers fourPasses @?= Unfinished

--chiefAndVice =
--  testCase "an uneven bid should return the chief and vice" $
--  playerBidsToStatus fivePlayers (singleCardRaise : twoCardRaise : fivePasses) @?=
--  (Finished . Result $ ChiefAndVice playerTwo playerOne)
--chiefOnly =
--  testGroup
--    "chiefOnly"
--    [ testCase "a single bid should return a chief only" $
--      playerBidsToStatus fivePlayers (singleCardRaise : fivePasses) @?=
--      (Finished . Result $ ChiefOnly playerOne)
--    , testCase "the same card values should return no vice" $
--      playerBidsToStatus
--        fivePlayers
--        (singleCardRaise : singleCardRaise2 : twoCardRaise : fivePasses) @?=
--      (Finished . Result $ ChiefOnly playerThree)
--    ]
redSeven = Card {suit = Red, rank = 7}

greenSeven = Card {suit = Green, rank = 7}

blueFive = Card {suit = Red, rank = 5}

blackOne = Card {suit = Black, rank = 1}

singleCardRaise = Raise [redSeven]

singleCardRaise2 = Raise [greenSeven]

twoCardRaise = Raise [blueFive, blackOne]

eklatCase :: TestTree
eklatCase =
  testCase
    "auction result should return eklat with the player at fault and the players affected" $
  playerBidsToStatus fivePlayers bids @?= eklat
  where
    bids = singleCardRaise : singleCardRaise : fivePasses
    eklat =
      Finished
        (NoResult
           (Eklat {atFault = playerTwo, affected = [playerOne], topBid = 1}))
