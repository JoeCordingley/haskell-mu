{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}

module New.GamePlay where

import           AuctionFunctions          (Chief (..))
import           AuctionPlay               (TrumpsAndPartner (..), ViceTrump, Partner(..))
import           Cards
import           Control.Lens
import           Control.Monad.Loops
import           Control.Monad.State.Class
import           Control.Monad.State.Lazy
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           New.Bidding
import           New.Players

data EndCondition
  = NumberOfRounds Int
  | ScoreGoal Int

playUntilScore ::
     (Monad m, Monoid (players score), Foldable1 players, Ord score)
  => score
  -> m (players score)
  -> m (players score)
playUntilScore goal playRound = playUntilScore' mempty
  where
    scoresMet = (goal >=) . getMax . foldMap1 Max
    playUntilScore' scores =
      if scoresMet scores
        then return scores
        else playAndAdd scores playRound >>= playUntilScore'

playSetNumberOfRounds :: (Monad m, Monoid scores) => Int -> m scores -> m scores
playSetNumberOfRounds num playRound =
  concatM (replicate num playRoundAndAdd) mempty
  where
    playRoundAndAdd scores = playAndAdd scores playRound

playAndAdd :: (Functor f, Semigroup scores) => scores -> f scores -> f scores
playAndAdd scores = fmap (scores <>)

data Stages f players player scores =
  Stages
    { dealCards :: f (players [Card])
    , runBidding :: player -> players [Card] -> f (FinishedBidding players player)
    , settleAuctionRound :: Chief player -> Maybe (Vice player) -> players CardPositions -> f (TrumpsAndPartner player)
    , cardPlay :: ChiefTrump -> Maybe ViceTrump -> Chief player -> players CardPositions -> f (players [Card])
    , scoreCardPlay :: Chief player -> ChiefTrump -> Maybe (Partner player) -> TopBid -> players [Card] -> scores
    , scoreStalemate :: Stalemate player -> scores
    }

gameRound :: (Monad f) => Stages f players player scores -> player -> f scores
gameRound (Stages dealCards runBidding settleAuction cardPlay scoreCardPlay scoreStalemate) firstPlayer = do
  finishedBids <- dealCards >>= runBidding firstPlayer
  case finishedBids of
    Successful (SuccessfulBidding chief vice topBid positions) -> do
      TrumpsAndPartner { chiefTrump, viceTrump, partner } <- settleAuction chief vice positions
      tricks <- cardPlay chiefTrump viceTrump chief positions
      return $ scoreCardPlay chief chiefTrump partner topBid tricks
    Unsuccessful stalemate -> return $ scoreStalemate stalemate

playMu ::
     (Monad m, Monoid (players Int), Foldable1 players, Cycling player)
  => Stages m players player (players Int)
  -> EndCondition
  -> player
  -> m (players Int)
playMu stages endCondition firstPlayer =
  evalStateT (playMatch endCondition stateful') firstPlayer
  where
    stateful' = players >>= lift . gameRound stages

playMatch ::
     (Monad m, Monoid (players Int), Foldable1 players)
  => EndCondition
  -> m (players Int)
  -> m (players Int)
playMatch (NumberOfRounds num) = playSetNumberOfRounds num
playMatch (ScoreGoal goal)     = playUntilScore goal
