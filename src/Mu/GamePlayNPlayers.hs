{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Mu.GamePlayNPlayers where

import Mu.GamePlay
import Mu.Players
import Data.Tuple.Homogenous
import Cards (Score)
import           Data.Semigroup.Foldable
import Control.Lens
import Mu.Deal
import           Control.Monad.Random.Class
import qualified Mu.Auction as Auction
import Mu.Auction (IsMoreThanThreePlayers(..))
import Mu.CardPlay
import qualified Mu.Scoring as Scoring

playMuThreePlayers :: MonadRandom m => 
  Dependencies m Tuple3 NOfThree -> EndCondition -> m (Tuple3 Score)
playMuThreePlayers (Dependencies getBid getViceTrump getChiefTrump getPartner getCard) endCondition = playMu getCardPositions stages endCondition OneOfThree where
  getCardPositions = view . threeLens 
  stages = Stages { dealCards, runBidding, settleAuctionRound, cardPlay, scoreCardPlay, scoreStalemate }
  dealCards = deal3
  runBidding = Auction.playAuctionAndRecord threePlayers threeLens getBid
  settleAuctionRound = Auction.settleAuctionRound (IsMoreThanThreePlayers False) getViceTrump getChiefTrump getPartner threePlayers
  cardPlay = playCardsStateful threeLens getCard (NumberOfRounds 12)
  scoreCardPlay = Scoring.scoreCardPlay threeLens threePlayers
  scoreStalemate = Scoring.scoreStalemate threeLens


playMuFourPlayers :: MonadRandom m => 
  Dependencies m Tuple4 NOfFour -> EndCondition -> m (Tuple4 Score)
playMuFourPlayers (Dependencies getBid getViceTrump getChiefTrump getPartner getCard) endCondition = playMu getCardPositions stages endCondition OneOfFour where
  getCardPositions = view . fourLens
  stages = Stages { dealCards, runBidding, settleAuctionRound, cardPlay, scoreCardPlay, scoreStalemate }
  dealCards = deal4
  runBidding = Auction.playAuctionAndRecord fourPlayers fourLens getBid
  settleAuctionRound = Auction.settleAuctionRound (IsMoreThanThreePlayers True) getViceTrump getChiefTrump getPartner fourPlayers
  cardPlay = playCardsStateful fourLens getCard (NumberOfRounds 15)
  scoreCardPlay = Scoring.scoreCardPlay fourLens fourPlayers
  scoreStalemate = Scoring.scoreStalemate fourLens


playMuFivePlayers :: MonadRandom m => 
  Dependencies m Tuple5 NOfFive -> EndCondition -> m (Tuple5 Score)
playMuFivePlayers (Dependencies getBid getViceTrump getChiefTrump getPartner getCard) endCondition = playMu getCardPositions stages endCondition OneOfFive where
  getCardPositions = view . fiveLens
  stages = Stages { dealCards, runBidding, settleAuctionRound, cardPlay, scoreCardPlay, scoreStalemate }
  dealCards = deal5
  runBidding = Auction.playAuctionAndRecord fivePlayers fiveLens getBid
  settleAuctionRound = Auction.settleAuctionRound (IsMoreThanThreePlayers True) getViceTrump getChiefTrump getPartner fivePlayers
  cardPlay = playCardsStateful fiveLens getCard (NumberOfRounds 12)
  scoreCardPlay = Scoring.scoreCardPlay fiveLens fivePlayers
  scoreStalemate = Scoring.scoreStalemate fiveLens

playMuSixPlayers :: MonadRandom m => 
  Dependencies m Tuple6 NOfSix -> EndCondition -> m (Tuple6 Score)
playMuSixPlayers (Dependencies getBid getViceTrump getChiefTrump getPartner getCard) endCondition = playMu getCardPositions stages endCondition OneOfSix where
  getCardPositions = view . sixLens
  stages = Stages { dealCards, runBidding, settleAuctionRound, cardPlay, scoreCardPlay, scoreStalemate }
  dealCards = deal6
  runBidding = Auction.playAuctionAndRecord sixPlayers sixLens getBid
  settleAuctionRound = Auction.settleAuctionRound (IsMoreThanThreePlayers True) getViceTrump getChiefTrump getPartner sixPlayers
  cardPlay = playCardsStateful sixLens getCard (NumberOfRounds 10)
  scoreCardPlay = Scoring.scoreCardPlay sixLens sixPlayers
  scoreStalemate = Scoring.scoreStalemate sixLens
