{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RankNTypes     #-}
module New.BiddingPlayerNumbers where

import New.Bidding (playAuctionAndRecord, CardPositions(..))
import AuctionFunctions (Bid(..))
import Util (minus)
import Cards (Card)
import New.Players
import Data.Tuple.Homogenous
import New.Bidding (FinishedBidding)
import           Control.Monad.State.Lazy (StateT(..))
import Control.Lens
import New.TupleInstances

type FirstPlayer a = a

biddingThreePlayers :: Monad f => (NOfThree -> [Card] -> f Bid) -> FirstPlayer NOfThree -> Tuple3 CardPositions -> f (FinishedBidding Tuple3 NOfThree)
biddingThreePlayers getBid firstPlayer cardPositions = playAuctionAndRecord getBidStateful' raiseThree tuple3N 3 firstPlayer cardPositions where 
  raiseThree n cards = set (threeLens n) cards $ all3 []
  tuple3N = Tuple3 (OneOfThree, TwoOfThree, ThreeOfThree)
  getBidStateful' = getBidStateful threeLens getBid

biddingFourPlayers :: Monad f => (NOfFour -> [Card] -> f Bid) -> FirstPlayer NOfFour -> Tuple4 CardPositions -> f (FinishedBidding Tuple4 NOfFour)
biddingFourPlayers getBid firstPlayer cardPositions = playAuctionAndRecord getBidStateful' raise tupleN 4 firstPlayer cardPositions where 
  raise n cards = set (fourLens n) cards $ all4 []
  tupleN = Tuple4 (OneOfFour, TwoOfFour, ThreeOfFour, FourOfFour)
  getBidStateful' = getBidStateful fourLens getBid

getBidStateful
  :: Functor m =>
     (p -> Lens' s CardPositions)
     -> (p -> [Card] -> m Bid)
     -> p
     -> StateT s m Bid
getBidStateful l getBid n = StateT $ getBidStateful' where
  getBidStateful' cardPositions = fmap (record' cardPositions) $ getBid n (inHand $ view (l n) cardPositions)
  record' cardPositions bid = (bid , over (l n) (record bid) cardPositions)


record :: Bid -> CardPositions -> CardPositions
record (Raise cards) (CardPositions inHand onTable ) = CardPositions (minus cards inHand) (cards <> onTable)
record Pass c = c



