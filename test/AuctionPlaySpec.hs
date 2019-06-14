module AuctionPlaySpec
  ( auctionPlayTests
  ) where

import           AuctionFunctions
import           AuctionPlay
import           Cards
import           Control.Arrow
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Data.Functor.Identity
import qualified Data.Map.Lazy            as Map
import qualified Data.Set                 as Set
import           Test.Tasty
import           Test.Tasty.HUnit

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

example3InitialHands :: Map.Map Player [Card]
example3InitialHands = Map.adjust init dagmar example1InitialHands

example1InitialState :: AuctionState Player
example1InitialState = initialState $ Map.toList example1InitialHands

example3InitialState :: AuctionState Player
example3InitialState = initialState $ Map.toList example3InitialHands

exampleInitialPlays :: [(Int, Player, Bid)]
exampleInitialPlays =
  [ (1, anna, Raise [redSix])
  , (2, beate, Pass)
  , (2, conny, Raise [yellowEight])
  , (2, dagmar, Raise [greenOne, blueOne])
  , (3, emma, Pass)
  , (2, anna, Raise [redTwo])
  , (3, beate, Raise [blueNine])
  , (2, conny, Raise [greenNine, redFive])
  , (2, dagmar, Pass)
  , (4, emma, Raise [redEight])
  , (2, anna, Raise [redNine])
  , (3, beate, Pass)
  , (1, conny, Pass)
  ]

example1Plays :: [(Int, Player, Bid)]
example1Plays =
  exampleInitialPlays ++
  [ (2, dagmar, Raise [greenEight, greenSeven])
  , (4, emma, Pass)
  , (2, anna, Pass)
  , (4, beate, Pass)
  , (2, conny, Pass)
  , (1, dagmar, Pass)
  ]

example3Plays =
  exampleInitialPlays ++
  [ (2, dagmar, Raise [greenEight])
  , (3, emma, Pass)
  , (1, anna, Pass)
  , (3, beate, Pass)
  , (1, conny, Pass)
  , (1, dagmar, Pass)
  ]

type TestState = State [(Int, Player, Bid)]

type TestContext = ExceptT TestFailure TestState

type TestResult = Either TestFailure

type TestFailure = String

noMoreActions = "no more actions"

wrongPlayerRequested = "wrong player requested"

wrongMaxRequested = "wrong max requested"

cardsNotAvailable = "cards not available"

unconsState :: State [a] (Maybe a)
unconsState = state uncons
  where
    uncons (a:as) = (Just a, as)
    uncons []     = (Nothing, [])

verifyBid :: Monad m => [Card] -> Bid -> ExceptT TestFailure m Bid
verifyBid availableCards (Raise cards)
  | all (\x -> elem x availableCards) cards = return $ Raise cards
  | otherwise = throwError cardsNotAvailable
verifyBid _ Pass = return Pass

verify :: Monad m => e -> Bool -> ExceptT e m ()
verify e False = throwError e
verify _ True  = return ()

nextBid :: Int -> Player -> [Card] -> TestContext Bid
nextBid max player cards = do
  maybeTuple <- lift unconsState
  case maybeTuple of
    Just (max', player', bid) ->
      verify wrongMaxRequested (max == max') *>
      verify wrongPlayerRequested (player == player') *>
      verifyBid cards bid
    Nothing -> throwError noMoreActions

runExample ::
     AuctionState Player
  -> [(Int, Player, Bid)]
  -> ( Either TestFailure (AuctionResult Player, AuctionState Player)
     , [(Int, Player, Bid)])
runExample initialState plays =
  runState (runBiddingInTestContext initialState) plays
  where
    runBiddingInTestContext = runExceptT . runStateT (bidding (nextBid) players)

exampleTest ::
     String
  -> Map.Map Player [Card]
  -> [(Int, Player, Bid)]
  -> (TestResult (AuctionResult Player) -> TestTree)
  -> TestTree
exampleTest name initialHands plays resultTest =
  testGroup name [usedAllPlays, resultTest result, noCardsLeft, cardsAllPlayed]
  where
    usedAllPlays = testCase "requested all plays" $ playsRemaining @?= []
    noCardsLeft =
      testCase "finished with no cards in hand" $
      cardsInHand' @?= Right emptyPlayerMap
    cardsAllPlayed =
      testCase "finished with all cards played" $
      right (Map.map Set.fromList) cardsPlayed @?=
      Right (Map.map Set.fromList initialHands)
    (exampleRun, playsRemaining) =
      runExample (initialState $ Map.toList initialHands) plays
    result = right fst exampleRun
    state = right snd exampleRun
    cardsInHand' = right (cardsInHand . auctionPositions) state
    cardsPlayed = right (cardsOnTable . auctionPositions) state
    runExample initialState plays =
      runState (runBiddingInTestContext initialState) plays
    runBiddingInTestContext = runExceptT . runStateT (bidding (nextBid) players)

auctionPlayTests :: TestTree
auctionPlayTests = testGroup "AuctionPlay" [biddingTests]

biddingTests :: TestTree
biddingTests = testGroup "bidding" [example1, example3]

example1 :: TestTree
example1 =
  exampleTest "example 1" example1InitialHands example1Plays example1Winner

example3 :: TestTree
example3 =
  exampleTest "example 3" example3InitialHands example3Plays example3ResultTest

example1Winner :: TestResult (AuctionResult Player) -> TestTree
example1Winner result =
  testCase "returned dagmar as winner and conny as vice" $
  testResult result @?= Right (ChiefAndVice dagmar conny)

testResult :: TestResult (AuctionResult Player) -> TestResult (Winners Player)
testResult e = e >>= winners
  where
    winners (Result winners _) = Right winners
    winners _                  = Left "not a result"

emptyPlayerMap :: Map.Map Player [Card]
emptyPlayerMap = Map.fromList $ map (\player -> (player, [])) players

example3ResultTest :: TestResult (AuctionResult Player) -> TestTree
example3ResultTest testResult =
  testGroup
    "returned eklat with dagmar at fault and only anna and conny affected"
    [dagmarAtFault, twoAffected, annaAffected, connyAffected]
  where
    dagmarAtFault = testCase "dagmar is at fault" $ testAtFault @?= Right dagmar
    annaAffected =
      testCase "anna affected" $ affectedContainsAnna @?= Right True
    connyAffected =
      testCase "conny affected" $ affectedContainsConny @?= Right True
    twoAffected = testCase "two players affected" $ numberOfAffected @?= Right 2
    testEklat = do
      res <- testResult
      case res of
        NoResult (eklat@Eklat {}) -> Right eklat
        _                         -> Left "not eklat"
    testAffected = fmap affected testEklat
    numberOfAffected = fmap length testAffected
    affectedContainsAnna = fmap (elem anna) testAffected
    affectedContainsConny = fmap (elem conny) testAffected
    testAtFault = fmap atFault testEklat
