module GamePlay where

import Scoring
import Data.Maybe
import Data.List
import qualified Data.Map.Lazy as Map
import Control.Monad.State.Lazy

data EndCondition = NumberOfRounds Int | ScoreGoal Int

playUntilScore :: Monad f => f (Scores player) -> Int -> f (Scores player)
playUntilScore playRound goal = firstMatch playRound (scoresMet goal)

playSetNumberOfRounds :: Monad f => f (Scores player) -> Int -> f (Scores player)
playSetNumberOfRounds playRound num = fmap last . sequence . replicate num $ playRound

firstMatch :: Monad f => f a -> (a -> Bool) -> f a
firstMatch f p = do
  a <- f
  if p a then return a else firstMatch f p

scoresMet :: Int -> Scores player -> Bool
scoresMet goal scores = goal >= (maximum $ Map.elems scores)

addScores :: Ord player => Scores player -> Scores player -> Scores player
addScores = Map.unionWith (+)

accumulateScores :: (Monad f, Ord player) => f (Scores player) -> StateT (Scores player) f (Scores player)
accumulateScores playRound = do
  scores <- lift playRound
  modifyAndReturn $ addScores scores

initialScores = Map.empty

playGame :: (Ord player, Monad f) => EndCondition -> ([player] -> f (Scores player)) -> [player] -> f (Scores player)
playGame (NumberOfRounds num) playRound players = evalStateT accumulatingScores initialScores  where
  accumulatingScores = playSetNumberOfRounds f num
  f = accumulateScores g
  g = evalStateT (playRoundAndChangeOrder playRound) players




playRoundAndChangeOrder :: (Monad f) => ([player] -> f (Scores player)) -> StateT [player] f (Scores player)
playRoundAndChangeOrder playRound = (get >>= (lift . playRound)) <* modify rotate


rotate :: [a] -> [a]
rotate (first:rest) = rest ++ [first]

modifyAndReturn :: MonadState s m => (s -> s) -> m s
modifyAndReturn f = modify f *> get


