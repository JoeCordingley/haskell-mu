module GamePlay where

import           AuctionPlay
import           Cards
import           Control.Monad.State.Lazy
import           Data.List
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
  => EndCondition
  -> [player]
  -> ([player] -> f (Scores player))
  -> f (Scores player)
playToTheEnd (NumberOfRounds num) players playRound =
  playToTheEnd' players playRound $ playSetNumberOfRounds num
playToTheEnd (ScoreGoal goal) players playRound =
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

gameRound ::
     (Monad f, Ord player)
  => ([player] ->  f [(player, [Card])])
  -> ([(player, [Card])] -> f (FinishedAuction player))
  -> (TrumpsAndTeams player -> f (CardsWon player))
  -> [player]
  -> f (Scores player)
gameRound dealCards playAuction playCards players = scoreFinishedRound numberOfPlayers <$> finishedRound where
  numberOfPlayers = length players
  finishedRound = dealCards players >>= playAuction >>= finishRound
  finishRound (Unsuccessful stalemate) =
    return $ FinishedViaStalemate stalemate
  finishRound (Successful trumpsAndTeams) =
    FinishedViaCardPlay trumpsAndTeams <$> playCards trumpsAndTeams

