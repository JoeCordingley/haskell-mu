module New.GamePlay
  ( CardPositions(..) ) where

import GamePlay (EndCondition(..))
import Control.Lens 
import Control.Monad.Loops
import Control.Monad.State.Class
import           Cards
import AuctionFunctions (Winners(..), Stalemate(..), chief)
import AuctionPlay (TrumpsAndChiefsTeam(..)) 
import Data.Semigroup

playUntilScore
  :: (Monad m, Monoid scores) =>
     Getting (Max Int) scores Int
     -> Int -> m scores -> m scores
playUntilScore fold goal playRound = playUntilScore' mempty
  where
    scoresMet = (goal >=) . maximum1Of fold 
    playUntilScore' scores = if scoresMet scores then return scores else playAndAdd scores playRound >>= playUntilScore'

playSetNumberOfRounds
  :: (Monad m, Monoid scores) => Int -> m scores -> m scores
playSetNumberOfRounds num playRound = concatM (replicate num playRoundAndAdd) mempty
  where playRoundAndAdd scores = playAndAdd scores playRound

playAndAdd :: (Functor f, Semigroup scores) => scores -> f scores -> f scores
playAndAdd scores = fmap (scores <>)

data SuccessfulBidding players player = SuccessfulBidding 
  { biddingWinners :: Winners player
  , successfulTopBid :: Int
  , biddingPositions :: players CardPositions
  }

data CardPositions = CardPositions 
  { inHand :: [Card] 
  , onTable :: [Card]
  }

data Stages f players player playerSequence = Stages 
  { dealCards :: f (players [Card])
  , runBidding :: players [Card] -> f (FinishedBidding players player)
  , settleAuctionRound :: Winners player -> players CardPositions ->  f (TrumpsAndChiefsTeam player)
  , cardPlay :: Trumps -> player -> players CardPositions -> f (players [Card])
  , scoreCardPlay :: TrumpsAndChiefsTeam player -> Int -> players [Card] -> players Int
  , scoreStalemate :: Stalemate player -> players Int
  , nextPlayer :: playerSequence -> playerSequence
  }

data FinishedBidding players player 
  = Successful (SuccessfulBidding players player)
  | Unsuccessful (Stalemate player)

gameRound :: 
     (MonadState playerSequence f)
  => Stages f players player playerSequence
  -> f (players Int)
gameRound (Stages dealCards runBidding settleAuction cardPlay scoreCardPlay scoreStalemate nextPlayer) = do
  finishedBids <- dealCards >>= runBidding
  scores <- case finishedBids of
    Successful (SuccessfulBidding winners topBid positions) -> do
      trumpsAndTeams <- settleAuction winners positions
      tricks <- cardPlay (trumps trumpsAndTeams) (chief winners) positions
      return $ scoreCardPlay trumpsAndTeams topBid tricks
    Unsuccessful stalemate -> return $ scoreStalemate stalemate 
  scores <$ modify nextPlayer

playMatch :: 
     (Monad m, Monoid scores) 
  => Getting (Max Int) scores Int
  -> EndCondition 
  -> m scores
  -> m scores
playMatch _ (NumberOfRounds num) = playSetNumberOfRounds num 
playMatch fold (ScoreGoal goal) = playUntilScore fold goal 
