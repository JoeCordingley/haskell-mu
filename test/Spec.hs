import           Auction
import           Cards
import           Control.Monad.State.Lazy
import           Data.List
import           Data.Maybe
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain (testGroup "tests" [passTest, sixtyCards, eklatNoPoints, unfinished, chiefAndVice, chiefOnly])

passTest :: TestTree
passTest = testCase "this should pass" (assertBool "true" True)

sixtyCards :: TestTree
sixtyCards = testCase "there should be 60 cards" (length fullDeck @?= 60)

playerOne = 1

playerTwo = 2
playerThree = 3

fivePlayers = [1 .. 5]

fivePasses = replicate 5 Pass
fourPasses = replicate 4 Pass

unconsState :: State [a] a
unconsState = state $ fromJust . uncons

--actions = Actions {getBid = \_ -> unconsState}

--playFivePlayers = evalState $ playAuction actions fivePlayers

eklatNoPoints :: TestTree
eklatNoPoints = testCase "five passes should be eklat no points" ( playerBidsToStatus fivePlayers fivePasses @?= Finished EklatNoPoints)

unfinished :: TestTree
unfinished = testCase "four passes should be unfinished" ( playerBidsToStatus fivePlayers fourPasses @?= Unfinished)

chiefAndVice = testCase "an uneven bid should return the chief and vice" ( playerBidsToStatus fivePlayers (singleCardRaise:twoCardRaise:fivePasses) @?= Finished  (ChiefAndVice  playerTwo playerOne))

chiefOnly = testGroup "chiefOnly" 
  [ testCase "a single bid should return a chief only" ( playerBidsToStatus fivePlayers (singleCardRaise:fivePasses) @?= Finished  (ChiefOnly  playerOne ))
  , testCase "the same card values should return no vice" ( playerBidsToStatus fivePlayers (singleCardRaise:singleCardRaise2:twoCardRaise:fivePasses) @?= Finished (ChiefOnly playerThree))
  ]


redSeven = Card {suit = Red, rank = 7}
greenSeven = Card {suit = Green, rank = 7}
blueFive = Card {suit = Red, rank = 5}
blackOne = Card {suit = Black, rank = 1}
singleCardRaise = Raise [redSeven]
singleCardRaise2 = Raise [greenSeven]
twoCardRaise = Raise [blueFive, blackOne]
--eklatCase :: TestTree
--eklatCase = testCase "auction result should return eklat with the player at fault and the players affected" (auctionResult fivePlayers bids @?= eklat) where
--  bids = singleCardRaise:singleCardRaise:fivePasses
--  eklat = Eklat{atFault=playerTwo, affected=[playerOne]}
