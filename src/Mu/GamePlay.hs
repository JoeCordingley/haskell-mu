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
import           Mu.CardPlay               (PlayableCard)
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
    , settleAuctionRound :: (Chief player, [Card]) -> Maybe ( Vice player
                                                            , [Card]) -> f (TrumpsAndPartner player)
    , cardPlay :: ChiefTrump -> Maybe ViceTrump -> Chief player -> players CardPositions -> f (players [Card])
    , scoreCardPlay :: Chief player -> ChiefTrump -> Maybe (Partner player) -> CardsBid -> players [Card] -> players Score
    , scoreStalemate :: Stalemate player -> players Score
    }

data Dependencies f players player =
  Dependencies
    { getBid        :: player -> [Card] -> f Bid
    , getViceTrump  :: Vice player -> [Card] -> f ViceTrump
    , getChiefTrump :: Chief player -> [Card] -> f ChiefTrump
    , getPartner    :: Chief player -> players player -> f (Partner player)
    , getCard       :: player -> [PlayableCard] -> f PlayableCard
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

playMatch ::
     (Monad m, Monoid (players Score), Foldable1 players)
  => EndCondition
  -> m (players Score)
  -> m (players Score)
playMatch (SetNumberOfRounds num) = playSetNumberOfRounds num
playMatch (ScoreGoal goal)     = playUntilScore goal
