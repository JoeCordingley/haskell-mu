{-# LANGUAGE TupleSections #-}

module BiddingResolutionSpec
  ( biddingResolutionTests
  , totalBidGenWithOutrightLeader
  ) where

import           BiddingResolution
import           Cards
import           Data.Map.Lazy         (Map)
import qualified Data.Map.Lazy         as Map
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Util

type Player = Int

biddingResolutionTests :: TestTree
biddingResolutionTests =
  testGroup "Bidding resolution tests" [noRaisesReturnsEklatNoPoints]

noRaisesReturnsEklatNoPoints :: TestTree
noRaisesReturnsEklatNoPoints =
  testCase "No raises returns EklatNoPoints" $
  (resolveBidding ([] :: [(Player, [Card])]) @?= EklatNoPoints)

--biddingWithOutRightLeaderReturnedAsChief :: TestTree
--biddingWithOutRightLeaderReturnedAsChief = testProperty "When one player has bid more cards than any other they should be returned as chief" $ forAll cases leaderShouldBe
cardGen :: Gen Card
cardGen = elements fullDeck

listOfN :: Int -> Gen a -> Gen [a]
listOfN = sequence .: replicate

listWithMaxSize :: Int -> Gen a -> Gen [a]
listWithMaxSize n gen = choose (0, n) >>= (flip listOfN gen)

bidsWithMaxSize :: Int -> [Player] -> Gen (Map Player [Card])
bidsWithMaxSize n players = Map.fromList <$> traverse pairWithGen players
  where
    pairWithGen player = (player, ) <$> listWithMaxSize n cardGen

totalBidGenWithOutrightLeader :: Gen (Player, Map Player [Card])
totalBidGenWithOutrightLeader = do
  numberOfPlayers <- choose (3, 6)
  let players = [1 .. numberOfPlayers]
  leader <- elements players
  leadersBid <- listOf1 cardGen
  let otherPlayers = remove leader players
      leadersBidSize = length leadersBid
  totalBids <-
    Map.insert leader leadersBid <$>
    bidsWithMaxSize (leadersBidSize + 1) otherPlayers
  return (leader, totalBids)

bidGenWithOutRightLeader :: Gen (Player, [(Player, [Card])])
bidGenWithOutRightLeader = do
  (leader, map) <- totalBidGenWithOutrightLeader
  (leader, ) <$> splitToBids map

groupIntoNonEmptys :: [a] -> Gen [[a]]
groupIntoNonEmptys [] = return []
groupIntoNonEmptys as = do
  i <- choose (1, length as)
  let (xs, ys) = splitAt i as
  (xs :) <$> groupIntoNonEmptys ys

splitToBids :: Map Player [Card] -> Gen [(Player, [Card])]
splitToBids cardMap = do
  nested <- traverse playerBids players
  shuffle $ concat nested
  where
    playerBids player =
      map (player, ) <$> (groupIntoNonEmptys $ findOrEmptyList player cardMap)
    players = Map.keys cardMap
--raisesGen :: Map Player [Card]
