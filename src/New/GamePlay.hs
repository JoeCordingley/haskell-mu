module New.GamePlay
  ( CardPositions(..)
  ) where

import           AuctionFunctions          (Winners (..), chief)
import           AuctionPlay               (TrumpsAndChiefsTeam (..))
import           Cards
import           Control.Lens
import           Control.Monad.Loops
import           Control.Monad.State.Class
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           GamePlay                  (EndCondition (..))
import           New.Bidding

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

data Stages f players player playerSequence = Stages
  { dealCards :: f (players [Card])
  , runBidding :: players [Card] -> f (FinishedBidding players player)
  , settleAuctionRound :: Winners player -> players CardPositions -> f (TrumpsAndChiefsTeam player)
  , cardPlay :: Trumps -> player -> players CardPositions -> f (players [Card])
  , scoreCardPlay :: TrumpsAndChiefsTeam player -> Int -> players [Card] -> players Int
  , scoreStalemate :: Stalemate player -> players Int
  , nextPlayer :: playerSequence -> playerSequence
  }

gameRound ::
     (MonadState playerSequence f)
  => Stages f players player playerSequence
  -> f (players Int)
gameRound (Stages dealCards runBidding settleAuction cardPlay scoreCardPlay scoreStalemate nextPlayer) = do
  finishedBids <- dealCards >>= runBidding
  scores <-
    case finishedBids of
      Successful (SuccessfulBidding winners topBid positions) -> do
        trumpsAndTeams <- settleAuction winners positions
        tricks <- cardPlay (trumps trumpsAndTeams) (chief winners) positions
        return $ scoreCardPlay trumpsAndTeams topBid tricks
      Unsuccessful stalemate -> return $ scoreStalemate stalemate
  scores <$ modify nextPlayer

playMatch ::
     (Monad m, Monoid (scores Int), Foldable1 scores)
  => EndCondition
  -> m (scores Int)
  -> m (scores Int)
playMatch (NumberOfRounds num) = playSetNumberOfRounds num
playMatch (ScoreGoal goal)     = playUntilScore goal
