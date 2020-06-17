{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Mu.GamePlay where

import           Cards
import           Control.Lens
import           Control.Monad.Loops
import           Control.Monad.State.Class
import           Control.Monad.State.Lazy
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Mu.Auction
import           Mu.CardPlay               (PlayableCard, WinnerOfTrick)
import           Mu.Players
import           Util                      (mapToSnd)

data EndCondition
  = SetNumberOfRounds Int
  | ScoreGoal Score

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

playUntilScoreWithUpdate ::
     (Monad m, Monoid (players score), Foldable1 players, Ord score)
  => (ScoreUpdate (players score) -> m ())
  -> score
  -> m (players score)
  -> m (players score)
playUntilScoreWithUpdate update goal playRound = playUntilScore' mempty
  where
    scoresMet = (goal >=) . getMax . foldMap1 Max
    playUntilScore' scores =
      if scoresMet scores
        then return scores
        else playAndAddAndUpdate update scores playRound >>= playUntilScore'

playSetNumberOfRounds :: (Monad m, Monoid scores) => Int -> m scores -> m scores
playSetNumberOfRounds num playRound =
  concatM (replicate num playRoundAndAdd) mempty
  where
    playRoundAndAdd scores = playAndAdd scores playRound

playSetNumberOfRoundsWithUpdate :: (Monad m, Monoid scores) => (ScoreUpdate scores -> m ()) -> Int -> m scores -> m scores
playSetNumberOfRoundsWithUpdate update num playRound =
  concatM (replicate num playRoundAndAdd) mempty
  where
    playRoundAndAdd scores = playAndAddAndUpdate update scores playRound

data ScoreUpdate scores = ScoreUpdate
  { roundScores :: scores
  , runningScores :: scores
  }

playAndAdd :: (Functor f, Semigroup scores) => scores -> f scores -> f scores
playAndAdd scores = fmap (scores <>)

playAndAddAndUpdate :: (Monad f, Semigroup scores) => (ScoreUpdate scores -> f ()) -> scores -> f scores -> f scores
playAndAddAndUpdate update previousScores round = do
  roundScores <- round
  let runningScores = roundScores <> previousScores
  update ScoreUpdate {roundScores, runningScores}
  return runningScores


data Stages f players player =
  Stages
    { dealCards :: f (players [Card])
    , runBidding :: player -> players [Card] -> f (FinishedBidding players player)
    , settleAuctionRound :: (Chief player, [Card]) -> Maybe ( Vice player
                                                            , [Card]) -> f (TrumpsAndPartner player)
    , cardPlay :: ChiefTrump -> Maybe ViceTrump -> Chief player -> players CardPositions -> f (players [Card])
    , scoreCardPlay :: Chief player -> ChiefTrump -> Maybe (Partner player) -> CardsBid -> players [Card] -> players Score
    , scoreStalemate :: Stalemate player -> players Score
    }

data Dependencies f players player =
  Dependencies
    { requestBid        :: player -> MaxRaise -> [Card] -> f Bid
    , requestViceTrump  :: Vice player -> [Trump] -> f ViceTrump
    , requestChiefTrump :: Chief player -> [Trump] -> f ChiefTrump
    , requestPartner    :: Chief player -> players player -> f (Partner player)
    , requestCard       :: player -> [PlayableCard] -> f PlayableCard
    }

data Updates f player scores = 
  Updates
    { dealUpdate :: player -> [Card] -> f () 
    , biddingResultUpdate :: BiddingResult player -> f ()
    , trickWinnerUpdate :: WinnerOfTrick player -> f ()
    , scoresUpdate :: ScoreUpdate scores -> f ()
    }

gameRound ::
     (Monad f, Functor players)
  => (player -> players CardPositions -> CardPositions)
  -> Stages f players player 
  -> player
  -> f (players Score)
gameRound f (Stages dealCards runBidding settleAuction cardPlay scoreCardPlay scoreStalemate) firstPlayer = do
  finishedBids <- dealCards >>= runBidding firstPlayer
  case finishedBids of
    Successful (SuccessfulBidding chief vice topBid positions) -> do
      let playerCards player = onTable $ f player positions
          chiefCards = playerCards (getChief chief)
          viceAndCards = fmap (mapToSnd (playerCards . getVice)) vice
      TrumpsAndPartner {chiefTrump, viceTrump, partner} <-
        settleAuction (chief, chiefCards) viceAndCards
      tricks <- cardPlay chiefTrump viceTrump chief positions
      return $ scoreCardPlay chief chiefTrump partner topBid tricks
    Unsuccessful stalemate -> return $ scoreStalemate stalemate


playMu ::
     ( Monad m
     , Monoid (players Score)
     , Foldable1 players
     , Cycling player
     , Functor players
     )
  => (player -> players CardPositions -> CardPositions)
  -> Stages m players player 
  -> EndCondition
  -> player
  -> m (players Score)
playMu f stages endCondition firstPlayer =
  evalStateT (playMatch endCondition stateful') firstPlayer
  where
    stateful' = players >>= lift . gameRound f stages

playMuWithUpdate ::
     ( Monad m
     , Monoid (players Score)
     , Foldable1 players
     , Cycling player
     , Functor players
     )
  => (ScoreUpdate (players Score) -> m ())
  -> (player -> players CardPositions -> CardPositions)
  -> Stages m players player 
  -> EndCondition
  -> player
  -> m (players Score)
playMuWithUpdate update f stages endCondition firstPlayer =
  evalStateT (playMatchWithUpdate (lift. update) endCondition stateful') firstPlayer
  where
    stateful' = players >>= lift . gameRound f stages

playMatch ::
     (Monad m, Monoid (players Score), Foldable1 players)
  => EndCondition
  -> m (players Score)
  -> m (players Score)
playMatch (SetNumberOfRounds num) = playSetNumberOfRounds num
playMatch (ScoreGoal goal)     = playUntilScore goal

playMatchWithUpdate ::
     (Monad m, Monoid (players Score), Foldable1 players)
  => (ScoreUpdate (players Score) -> m ())
  -> EndCondition
  -> m (players Score)
  -> m (players Score)
playMatchWithUpdate update (SetNumberOfRounds num) = playSetNumberOfRoundsWithUpdate update num
playMatchWithUpdate update (ScoreGoal goal)     = playUntilScoreWithUpdate update goal
