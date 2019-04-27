module AuctionPlaySpec
  ( auctionPlayTests
  ) where

import           AuctionFunctions
import           AuctionPlay
import           Cards
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Functor.Identity
import qualified Data.Map.Lazy            as Map
import qualified Data.Set                 as Set
import           Test.Tasty
import           Test.Tasty.HUnit
import Control.Arrow

testThisWorks :: TestTree
testThisWorks = testCase "1 + 1" (1 + 1 @?= 3)

type Player = String

players :: [Player]
players = [anna, beate, conny, dagmar, emma]

anna = "Anna"

beate = "Beate"

conny = "Conny"

dagmar = "Dagmar"

emma = "Emma"

redSix = Card Red 6

yellowEight = Card Yellow 8

greenOne = Card Green 1

blueOne = Card Blue 1

redTwo = Card Red 2

blueNine = Card Blue 9

greenNine = Card Green 9

redFive = Card Red 5

redEight = Card Red 8

redNine = Card Red 9

greenEight = Card Green 8

greenSeven = Card Green 7

example1InitialHands :: Map.Map Player [Card]
example1InitialHands = 
  Map.fromList
    [ (anna, [redSix, redTwo, redNine])
    , (beate, [blueNine])
    , (conny, [yellowEight, greenNine, redFive])
    , (dagmar, [greenOne, blueOne, greenEight, greenSeven])
    , (emma, [redEight])
    ]

example1InitialState :: AuctionState Player
example1InitialState =
  initialState example1InitialHands 

example1Plays :: [(Player, Bid)]
example1Plays =
  [ (anna, Raise [redSix])
  , (beate, Pass)
  , (conny, Raise [yellowEight])
  , (dagmar, Raise [greenOne, blueOne])
  , (emma, Pass)
  , (anna, Raise [redTwo])
  , (beate, Raise [blueNine])
  , (conny, Raise [greenNine, redFive])
  , (dagmar, Pass)
  , (emma, Raise [redEight])
  , (anna, Raise [redNine])
  , (beate, Pass)
  , (conny, Pass)
  , (dagmar, Raise [greenEight, greenSeven])
  , (emma, Pass)
  , (anna, Pass)
  , (beate, Pass)
  , (conny, Pass)
  , (dagmar, Pass)
  ]

type TestState = State [(Player, Bid)]

type TestContext = ExceptT TestFailure TestState

type TestResult = Either TestFailure

data TestFailure
  = NoMoreActions
  | WrongPlayerRequested
  | WrongMaxRequested
  | CardsNotAvailable deriving (Eq, Show)


unconsState ::  State [a] (Maybe a)
unconsState = state uncons where
  uncons (a:as) = (Just a,as)
  uncons [] = (Nothing, [])


verifyBid :: Monad m => [Card] -> Bid -> ExceptT TestFailure m Bid
verifyBid availableCards (Raise cards)
  | all (\x -> elem x availableCards) cards = return $ Raise cards
  | otherwise = throwError CardsNotAvailable
verifyBid _ Pass = return Pass

nextBid :: Player -> [Card] -> TestContext Bid
nextBid correctPlayer availableCards = do
  maybeA <- lift unconsState
  case maybeA of
    Just (player, bid)
      | player == correctPlayer -> verifyBid availableCards bid
      | otherwise -> throwError WrongPlayerRequested
    Nothing -> throwError NoMoreActions


runBiddingInTestContext ::
     AuctionState Player
  -> TestState (Either TestFailure (AuctionResult Player, AuctionState Player))
runBiddingInTestContext =
  runExceptT . runStateT (bidding (const nextBid) players)

(example1Run, example1PlaysRemaining) =
  runState (runBiddingInTestContext example1InitialState) example1Plays :: (Either TestFailure (AuctionResult Player, AuctionState Player), [(Player, Bid)])

example1Result :: TestResult (AuctionResult Player)
example1Result = right fst example1Run

example1State :: TestResult (AuctionState Player)
example1State = right snd example1Run

example1CardsInHand :: TestResult (Map.Map Player [Card])
example1CardsInHand = right cardsInHand example1State

example1CardsPlayed :: TestResult (Map.Map Player [Card])
example1CardsPlayed = right cardsBid example1State

auctionPlayTests :: TestTree 
auctionPlayTests = testGroup "AuctionPlay" [biddingTests]

biddingTests :: TestTree
biddingTests = testGroup "bidding" [example1]

example1 :: TestTree
example1 = testGroup "example1" [example1UsedAllPlays, example1WinnerTest, example1CardsLeftTest, example1CardsPlayedTest]

example1UsedAllPlays :: TestTree
example1UsedAllPlays =
  testCase "requested all plays" $ example1PlaysRemaining @?= []

example1WinnerTest :: TestTree
example1WinnerTest =
  testCase "returned dagmar as winner and conny as vice" $ example1Result @?= Right(Result (ChiefAndVice dagmar conny) )

emptyPlayerMap :: Map.Map Player [Card]
emptyPlayerMap = Map.fromList $ map (\player -> (player,[])) players

example1CardsLeftTest :: TestTree
example1CardsLeftTest = testCase "finished with no cards in hand" $ example1CardsInHand @?= Right emptyPlayerMap

example1CardsPlayedTest :: TestTree
example1CardsPlayedTest = testCase "finished with all cards played" $ right (Map.map Set.fromList) example1CardsPlayed @?= Right (Map.map Set.fromList example1InitialHands)
