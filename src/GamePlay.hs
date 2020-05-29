module GamePlay
  ( EndCondition(..)
  ) where

import           AuctionFunctions         (Bid (..), CardPositions,
                                           FinishedBidding (..), Stalemate,
                                           SuccessfulBidding (..), Winners,
                                           chief)
import           CardPlay                 (PlayableCard, playCards)
import           Cards
import           Control.Monad.State.Lazy
import           Data.List
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map
import           Data.Maybe
import           Scoring

data EndCondition
  = NumberOfRounds Int
  | ScoreGoal Int

playUntilScore :: Monad f => Int -> f (Scores player) -> f (Scores player)
playUntilScore goal playRound = firstMatch playRound (scoresMet goal)

playSetNumberOfRounds ::
     Monad f => Int -> f (Scores player) -> f (Scores player)
playSetNumberOfRounds num playRound =
  fmap last . sequence . replicate num $ playRound

firstMatch :: Monad f => f a -> (a -> Bool) -> f a
firstMatch f p = do
  a <- f
  if p a
    then return a
    else firstMatch f p

scoresMet :: Int -> Scores player -> Bool
scoresMet goal scores = goal >= (maximum $ Map.elems scores)

addScores :: Ord player => Scores player -> Scores player -> Scores player
addScores = Map.unionWith (+)

initialScores = Map.empty

playToTheEnd ::
     (Ord player, Monad f)
  => ([player] -> f (Scores player))
  -> EndCondition
  -> [player]
  -> f (Scores player)
playToTheEnd playRound (NumberOfRounds num) players =
  playToTheEnd' players playRound $ playSetNumberOfRounds num
playToTheEnd playRound (ScoreGoal goal) players =
  playToTheEnd' players playRound $ playUntilScore goal

playToTheEnd' players playRound finish =
  evalStateT stateful (players, initialScores)
  where
    stateful = finish $ playRoundAndUpdate playRound

playRoundAndUpdate ::
     (Monad f, Ord player)
  => ([player] -> f (Scores player))
  -> StateT ([player], Scores player) f (Scores player)
playRoundAndUpdate playRound = do
  (players, previousScores) <- get
  theseScores <- lift . playRound $ players
  let newScore = addScores previousScores theseScores
  put (rotate players, newScore)
  return newScore

rotate :: [a] -> [a]
rotate (first:rest) = rest ++ [first]

type DealCards f player = ([player] -> f [(player, [Card])])

--dependencies ::
--     (Ord player, Monad f)
--  => ([player] -> f [(player, [Card])])
--  -> (Int -> player -> [Card] -> f Bid)
--  -> (player -> [Trump] -> f Trump)
--  -> (player -> [player] -> f player)
--  -> (player -> [PlayableCard] -> f PlayableCard)
--  -> Dependencies f player
--dependencies dealCards getBid getTrump getPartner getCard =
--  Dependencies
--    { dealCards = dealCards
--    , runBidding = bidding2 getBid
--    , settleAuctionRound = settleAuction getTrump getPartner
--    , cardPlay = playCards getCard
--    }

data Dependencies f player =
  Dependencies
    { dealCards :: [player] -> f [(player, [Card])]
    , runBidding :: [(player, [Card])] -> f (FinishedBidding player)
    , settleAuctionRound :: [player] -> Winners player -> CardPositions player -> f (TrumpsAndChiefsTeam player)
    , cardPlay :: Trumps -> player -> [player] -> CardPositions player -> f (Map player [Card])
    }

--gameRound ::
--     (Monad f, Ord player)
--  => Dependencies f player
--  -> (TrumpsAndChiefsTeam player -> TopBid -> Map player [Card] -> Scores player)
--  -> [player]
--  -> f (Scores player)
--gameRound (Dependencies dealCards runBidding settleAuction cardPlay) scoreCardPlay players = do
--  finishedBids <- dealCards players >>= runBidding
--  case finishedBids of
--    Successful (SuccessfulBidding winners topBid positions) -> do
--      trumpsAndTeams <- settleAuction players winners positions
--      tricks <-
--        cardPlay (trumps trumpsAndTeams) (chief winners) players positions
--      return $ scoreCardPlay trumpsAndTeams topBid tricks
--    Unsuccessful stalemate -> return $ stalemateScores stalemate

--play ::
--     (Monad f, Ord player)
--  => Dependencies f player
--  -> EndCondition
--  -> [player]
--  -> f (Scores player)
--play dependencies endCondition players =
--  playToTheEnd gameRound' endCondition players
--  where
--    gameRound' = gameRound dependencies (scoreCardPlay (length players))

type TopBid = Int
