{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Mu.Scoring where

import           Cards              (Card, ChiefTrump (..), Score (..),
                                     Trump (..), points, rank)
import           Control.Lens       hiding ((<|))
import           Data.Foldable      (find)
import           Data.List.NonEmpty ((<|))
import           Data.Monoid        (Endo (..))
import           Data.Semigroup     (Sum (..))
import           Mu.Auction         (Chief (..), Stalemate (..), 
                                     TrumpsAndPartner (..), CardsBid(..), Partner(..))

newtype NumberOfPlayers =
  NumberOfPlayers Int

scoreStalemate ::
     (Monoid scores, Eq player)
  => (player -> ASetter' scores Score)
  -> Stalemate player
  -> scores
scoreStalemate _ EklatNoPoints = mempty
scoreStalemate l Eklat {topBid = CardsBid topBid, atFault, affected} =
  setScores mempty
  where
    setScores = appEndo $ foldMap Endo setters
    setAtFault = set (l atFault) (Score (-10 * topBid))
    setAffected = fmap (\p -> set (l p) (Score (5 * topBid))) affected
    setters = setAtFault <| setAffected

scoreCardPlay ::
     (Semigroup (f Score), Foldable t, Eq player, Functor f, Foldable f)
  => (player -> Getting Score (f Score) Score)
  -> f player
  -> Chief player
  -> ChiefTrump
  -> Maybe (Partner player)
  -> CardsBid
  -> f (t Card)
  -> f Score
scoreCardPlay l players (Chief chief) (ChiefTrump chiefTrump) partner cardsBid cardsWon =
  (cardPoints <> bonusPoints)
  where
    cardPoints = (fmap . foldMap) (points . rank) cardsWon
    bonusPoints = fmap setBonus players
    chiefsTeamScores = chiefScore <> partnerScore
    chiefScore = view (l chief) cardPoints
    partnerScore = foldMap (\(Partner p) -> view (l p) cardPoints) partner
    isChief player = player == chief
    isPartner player = elem (Partner player) partner
    isChiefsTeam player = isChief player || isPartner player
    numberOfPlayers = NumberOfPlayers $ length players
    setBonus player =
      case goalResult numberOfPlayers cardsBid chiefsTeamScores of
        RankAchieved ->
          if isChiefsTeam player
            then chiefTeamBonus chiefTrump cardsBid
            else 0
        RankMissedBy (CardsBid n)
          | isChief player -> Score (-10 * n)
          | isPartner player -> 0
          | otherwise -> Score (5 * n)

chiefTeamBonus :: Trump -> CardsBid -> Score
chiefTeamBonus (SuitTrump _) (CardsBid cardsBid) = Score (10 * cardsBid)
chiefTeamBonus (RankTrump rank) (CardsBid cardsBid)
  | rank == 1 || rank == 7 = Score (10 + 10 * cardsBid)
  | otherwise = Score (20 + 10 * cardsBid)
chiefTeamBonus NoTrump (CardsBid cardsBid) = Score (30 + 10 * cardsBid)

data GoalResult
  = RankAchieved
  | RankMissedBy CardsBid

goalResult :: NumberOfPlayers -> CardsBid -> Score -> GoalResult
goalResult numberOfPlayers cardsBid score =
  if rankAchieved >= cardsBid
    then RankAchieved
    else RankMissedBy (cardsBid - rankAchieved)
  where
    isAchieved rank = score >= target numberOfPlayers rank
    rankAchieved =
      case find isAchieved [cardsBid,(cardsBid - 1) .. 1] of
        Just x  -> x
        Nothing -> 0

target :: NumberOfPlayers -> CardsBid -> Score
target (NumberOfPlayers 3) (CardsBid cardsBid) = Score (10 + 2 * cardsBid)
target (NumberOfPlayers 4) (CardsBid cardsBid) = Score (28 + 2 * cardsBid)
target (NumberOfPlayers 5) (CardsBid cardsBid) = Score (22 + 3 * cardsBid)
target (NumberOfPlayers 6) (CardsBid cardsBid) = Score (18 + 4 * cardsBid)
