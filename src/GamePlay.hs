module GamePlay where

import           Cards
import           Control.Monad.State.Lazy
import           Data.List
import qualified Data.Map.Lazy            as Map
import           Data.Map.Lazy            (Map)
import           Data.Maybe
import           Scoring
import AuctionFunctions (Stalemate, CardPositions, Winners, SuccessfulBidding(..), FinishedBidding(..))
import CardPlay (CardPlayState)

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


data Dependencies f player = Dependencies 
  { dealCards :: [player] ->  f [(player, [Card])]
  , runBidding :: [(player, [Card])] -> f (FinishedBidding player)
  , settleAuction :: SuccessfulBidding player -> f (TrumpsAndChiefsTeam player)
  , cardPlay :: Trumps -> [player] -> CardPositions player -> f (Map player [Card])
  }

gameRound ::
     (Monad f, Ord player)
  => Dependencies f player
  -> (TrumpsAndChiefsTeam player -> TopBid -> Map player [Card] -> Scores player)
  -> [player]
  -> f (Scores player)
gameRound (Dependencies dealCards runBidding settleAuction cardPlay) scoreCardPlay players = do
  finishedBids <- dealCards players >>= runBidding
  case finishedBids of
    Successful biddingResults -> do
      trumpsAndTeams <- settleAuction biddingResults
      tricks <- cardPlay (trumps trumpsAndTeams) players $ biddingPositions biddingResults
      return $ scoreCardPlay trumpsAndTeams (successfulTopBid biddingResults) tricks
    Unsuccessful stalemate -> return $ stalemateScores stalemate

play :: 
     (Monad f, Ord player)
  => Dependencies f player
  -> EndCondition
  -> [player]
  -> f (Scores player)
play dependencies endCondition players = playToTheEnd gameRound' endCondition players
  where
    gameRound' = gameRound dependencies (scoreCardPlay (length players)) 


type TopBid = Int

data Bid
  = Pass
  | Raise [Card]
  deriving (Show)


