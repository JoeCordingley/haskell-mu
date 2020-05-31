{-# LANGUAGE TupleSections #-}

module New.GamePlay where

import           AuctionFunctions          (Chief (..), Winners (..), chief)
import           AuctionPlay               (TrumpsAndChiefsTeam (..))
import           Cards
import           Control.Lens
import           Control.Monad.Loops
import           Control.Monad.State.Class
import           Control.Monad.State.Lazy
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           GamePlay                  (EndCondition (..))
import           New.Bidding
import           New.Players

playUntilScore ::
     (Monad m, Monoid (scores Int), Foldable1 scores)
  => Int
  -> m (scores Int)
  -> m (scores Int)
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

data Stages f players player =
  Stages
    { dealCards :: f (players [Card])
    , runBidding :: player -> players [Card] -> f (FinishedBidding players player)
    , settleAuctionRound :: Winners player -> players CardPositions -> f (TrumpsAndChiefsTeam player)
    , cardPlay :: Trumps -> Chief player -> players CardPositions -> f (players [Card])
    , scoreCardPlay :: TrumpsAndChiefsTeam player -> Int -> players [Card] -> players Int
    , scoreStalemate :: Stalemate player -> players Int
    }

gameRound :: (Monad f) => Stages f players player -> player -> f (players Int)
gameRound (Stages dealCards runBidding settleAuction cardPlay scoreCardPlay scoreStalemate) firstPlayer = do
  finishedBids <- dealCards >>= runBidding firstPlayer
  case finishedBids of
    Successful (SuccessfulBidding winners topBid positions) -> do
      trumpsAndTeams <- settleAuction winners positions
      tricks <- cardPlay (trumps trumpsAndTeams) (chief winners) positions
      return $ scoreCardPlay trumpsAndTeams topBid tricks
    Unsuccessful stalemate -> return $ scoreStalemate stalemate

playMu ::
     (Monad m, Monoid (scores Int), Foldable1 scores, Cycling player)
  => Stages m scores player
  -> EndCondition
  -> player
  -> m (scores Int)
playMu stages endCondition firstPlayer =
  evalStateT (playMatch endCondition stateful') firstPlayer
  where
    stateful' = players >>= lift . gameRound stages

playMatch ::
     (Monad m, Monoid (scores Int), Foldable1 scores)
  => EndCondition
  -> m (scores Int)
  -> m (scores Int)
playMatch (NumberOfRounds num) = playSetNumberOfRounds num
playMatch (ScoreGoal goal)     = playUntilScore goal
