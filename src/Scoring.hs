module Scoring
  ( Scores
  , FinishedRound(..)
  , CardsWon
  , scoreFinishedRound
  , TrumpsAndChiefsTeam(..)
  , scoreCardPlay
  ) where

import           AuctionFunctions
import           AuctionPlay
import           Cards
import           Data.List
import           Data.Map.Lazy    (Map)
import qualified Data.Map.Lazy    as Map
import           Util

type NumberOfPlayers = Int

type CardsBid = Int

type Score = Int

pairWith = flip (,)

target :: NumberOfPlayers -> CardsBid -> Score
target 3 cardsBid = 10 + 2 * cardsBid
target 4 cardsBid = 28 + 2 * cardsBid
target 5 cardsBid = 22 + 3 * cardsBid
target 6 cardsBid = 18 + 4 * cardsBid

bonusAvailable :: Trump -> CardsBid -> Score
bonusAvailable (SuitTrump _) cardsBid = 10 * cardsBid
bonusAvailable (RankTrump rank) cardsBid
  | rank == 1 || rank == 7 = 10 + 10 * cardsBid
  | otherwise = 20 + 10 * cardsBid
bonusAvailable NoTrump cardsBid = 30 + 10 * cardsBid

stalemateScores :: (Ord player) => Stalemate player -> Scores player
stalemateScores EklatNoPoints = Map.empty
stalemateScores Eklat { atFault = atFault
                      , affected = affected
                      , topBid = cardsBid
                      } =
  Map.fromList $
  (atFault, -10 * cardsBid) : map (pairWith $ 5 * cardsBid) affected

rankMissedBy :: NumberOfPlayers -> CardsBid -> Score -> Int
rankMissedBy numberOfPlayers cardsBid score = cardsBid - rankAchieved
  where
    rankAchieved =
      case find isAchieved [cardsBid,(cardsBid - 1) .. 1] of
        Just x  -> x
        Nothing -> 0
    isAchieved rank = score >= target numberOfPlayers rank

countPips :: CardsWon player -> Scores player
countPips = Map.map countIndividualPips
  where
    countIndividualPips = sum . map (points . rank)

bonusPoints ::
     (Ord player)
  => NumberOfPlayers
  -> Trump
  -> Teams player
  -> CardsBid
  -> Scores player
  -> Scores player
bonusPoints numberOfPlayers chiefTrump teams cardsBid cardPoints =
  case teams of
    ChiefAlone chief ->
      Map.fromList $
      (chief, chiefBonus') : map (pairWith opponentBonus') opponents
      where chiefBonus' = chiefBonus teamPoints
            opponentBonus' = opponentBonus teamPoints
            opponents = remove chief players
            teamPoints = playerPoints chief
    ChiefAndPartner chief partner ->
      Map.fromList $
      (chief, chiefBonus') :
      (partner, partnerBonus') : map (pairWith opponentBonus') opponents
      where opponents = remove chief $ remove partner players
            opponentBonus' = opponentBonus teamPoints
            chiefBonus' = chiefBonus teamPoints
            partnerBonus' = partnerBonus teamPoints
            teamPoints = playerPoints chief + playerPoints partner
  where
    players = Map.keys cardPoints
    playerPoints player = Map.findWithDefault 0 player cardPoints
    rankAchieved score = score >= target numberOfPlayers cardsBid
    rankMissedBy' = rankMissedBy numberOfPlayers cardsBid
    chiefBonus teamPoints =
      if rankAchieved teamPoints
        then chiefTeamBonus
        else -10 * rankMissedBy' teamPoints
    partnerBonus teamPoints =
      if rankAchieved teamPoints
        then chiefTeamBonus
        else 0
    opponentBonus teamPoints =
      if rankAchieved teamPoints
        then 0
        else 5 * rankMissedBy' teamPoints
    chiefTeamBonus = bonusAvailable chiefTrump cardsBid

scoreFinishedRound ::
     (Ord player) => NumberOfPlayers -> FinishedRound player -> Scores player
scoreFinishedRound numberOfPlayers (FinishedViaStalemate stalemate) =
  stalemateScores stalemate
scoreFinishedRound numberOfPlayers (FinishedViaCardPlay (TrumpsAndTeams trumps teams cardsBid _) cardsWon) =
  Map.unionWith (+) cardPoints bonusPoints'
  where
    cardPoints = countPips cardsWon
    bonusPoints' =
      bonusPoints numberOfPlayers (chiefTrump trumps) teams cardsBid cardPoints


data TrumpsAndChiefsTeam player
  = TrumpsAndChiefsTeam 
    { trumps :: Trumps
    , chiefsTeam :: Teams player
    }

scoreCardPlay :: (Ord player) => NumberOfPlayers -> TrumpsAndChiefsTeam player -> TopBid -> Map player [Card] -> Scores player
scoreCardPlay numberOfPlayers (TrumpsAndChiefsTeam trumps team) cardsBid cardsWon = 
  Map.unionWith (+) cardPoints bonusPoints'
  where
    cardPoints = countPips cardsWon
    bonusPoints' =
      bonusPoints numberOfPlayers (chiefTrump trumps) team cardsBid cardPoints


chiefTrump :: Trumps -> Trump
chiefTrump (SingleTrump trump)    = trump
chiefTrump (HigherLower higher _) = higher

type Scores player = Map player Score

data FinishedRound player
  = FinishedViaStalemate (Stalemate player)
  | FinishedViaCardPlay (TrumpsAndTeams player)
                        (CardsWon player)

type CardsWon player = Map player [Card]
