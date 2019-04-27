module AuctionPlaySpec(testThisWorks, example1UsedAllPlays) where

import           Test.Tasty
import           Test.Tasty.HUnit
import  AuctionFunctions
import AuctionPlay
import Cards
import Control.Monad.State.Lazy
import Control.Monad.Except
import Data.List
import Data.Functor.Identity
import qualified Data.Map.Lazy as Map

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

example1InitialState :: AuctionState Player
example1InitialState = initialState $ Map.fromList 
  [ (anna, [redSix,redTwo,redNine])
  , (beate, [blueNine])
  , (conny, [yellowEight, greenNine, redFive])
  , (dagmar, [greenOne, blueOne, greenEight, greenSeven])
  , (emma, [redEight])
  ]

example1Plays :: [(Player, Bid)]
example1Plays  = 
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
data TestFailure = NoMoreActions | WrongPlayerRequested | CardsNotAvailable

unconsState :: Monad m => StateT [a] m (Maybe a)
unconsState = do
  maybePair <- gets uncons
  case maybePair of
    Just (x , xs) -> put xs *> return (Just x)
    Nothing -> return Nothing

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

runBiddingInTestContext :: AuctionState Player ->  TestState (Either TestFailure (AuctionResult Player, AuctionState Player))
runBiddingInTestContext = runExceptT . runStateT (bidding (const nextBid) players) 

(example1result , example1PlaysRemaining) = runState (runBiddingInTestContext example1InitialState) example1Plays

example1UsedAllPlays :: TestTree
example1UsedAllPlays = testCase "example 1 used requested all plays" (example1PlaysRemaining @?= [])




