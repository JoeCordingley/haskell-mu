{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module New.BiddingPlayerNumbers where

import           AuctionFunctions         (Bid (..))
import           Cards                    (Card)
import           Control.Lens
import           Control.Monad.State.Lazy (StateT (..))
import           Data.Semigroup.Foldable
import           Data.Tuple.Homogenous
import           New.Bidding              (CardPositions (..),
                                           playAuctionAndRecord)
import           New.Bidding              (FinishedBidding)
import           New.Players
import           New.TupleInstances
import           Util                     (minus)

type FirstPlayer a = a

biddingNPlayers ::
     ( Foldable1 players
     , Traversable players
     , Eq player
     , Monad m
     , Monoid (players [Card])
     , Cycling player
     , Applicative players
     )
  => players player
  -> (forall c. player -> Lens' (players c) c)
  -> (player -> [Card] -> m Bid)
  -> player
  -> players CardPositions
  -> m (FinishedBidding players player)
biddingNPlayers tupleN nLens getBid firstPlayer cardPositions =
  playAuctionAndRecord
    getBidStateful'
    raiseThree
    tupleN
    firstPlayer
    cardPositions
  where
    raiseThree n cards = set (nLens n) cards $ mempty
    getBidStateful' n = StateT $ getBidStateful'''
      where
        getBidStateful''' cardPositions =
          fmap (record' cardPositions) $
          getBid n (inHand $ view (nLens n) cardPositions)
        record' cardPositions bid =
          (bid, over (nLens n) (record bid) cardPositions)

biddingThreePlayers ::
     Monad f
  => (NOfThree -> [Card] -> f Bid)
  -> FirstPlayer NOfThree
  -> Tuple3 CardPositions
  -> f (FinishedBidding Tuple3 NOfThree)
biddingThreePlayers =
  biddingNPlayers
    (Tuple3 (OneOfThree, TwoOfThree, ThreeOfThree))
    threeLens

biddingFourPlayers ::
     Monad f
  => (NOfFour -> [Card] -> f Bid)
  -> FirstPlayer NOfFour
  -> Tuple4 CardPositions
  -> f (FinishedBidding Tuple4 NOfFour)
biddingFourPlayers =
  biddingNPlayers
    (Tuple4 (OneOfFour, TwoOfFour, ThreeOfFour, FourOfFour))
    fourLens

biddingFivePlayers ::
     Monad f
  => (NOfFive -> [Card] -> f Bid)
  -> FirstPlayer NOfFive
  -> Tuple5 CardPositions
  -> f (FinishedBidding Tuple5 NOfFive)
biddingFivePlayers =
  biddingNPlayers
    (Tuple5 (OneOfFive, TwoOfFive, ThreeOfFive, FourOfFive, FiveOfFive))
    fiveLens

biddingSixPlayers ::
     Monad f
  => (NOfSix -> [Card] -> f Bid)
  -> FirstPlayer NOfSix
  -> Tuple6 CardPositions
  -> f (FinishedBidding Tuple6 NOfSix)
biddingSixPlayers =
  biddingNPlayers
    (Tuple6 (OneOfSix, TwoOfSix, ThreeOfSix, FourOfSix, FiveOfSix, SixOfSix))
    sixLens

record :: Bid -> CardPositions -> CardPositions
record (Raise cards) (CardPositions inHand onTable) =
  CardPositions (minus cards inHand) (cards <> onTable)
record Pass c = c
