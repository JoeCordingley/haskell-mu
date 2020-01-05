module GamePlay where

import           Cards
import           Control.Monad.State.Lazy
import           Data.List
import qualified Data.Map.Lazy            as Map
import           Data.Map.Lazy            (Map)
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




data Stalemate player 
  = EklatNoBids
  | Eklat
  { playerAtFault :: player
  , playersAffected :: [player]
  }
  
scoreStalemate ::
     Stalemate player
  -> Scores player
scoreStalemate = undefined

data CardPositions player = CardPositions
  { cardsInHand  :: Map player [Card]
  , cardsOnTable :: Map player [Card]
  } deriving (Eq, Show)



data Winners player = 
  Winners 
    { topBidder :: player
    , vice :: Maybe player
    }


data FinishedBidding player
  = Successful (SuccessfulBidding player)
  | Unsuccessful (Stalemate player)

data SuccessfulBidding player = SuccessfulBidding 
  { winners :: Winners player
  , winningBid :: Int
  , cardPositions :: CardPositions player
  }


gameRound ::
     (Monad f, Ord player)
  => ([player] -> f [(player, [Card])])
  -> ([(player, [Card])] -> f (FinishedBidding player))
  -> (SuccessfulBidding player -> f (TrumpsAndChiefsTeam player)) 
  -> (Trumps -> CardPositions player -> f (Map player [Card]))
  -> (TrumpsAndChiefsTeam player -> TopBid -> Map player [Card] -> Scores player)
  -> [player]
  -> f (Scores player)
gameRound dealCards runBidding settleAuction cardPlay scoreCardPlay players = do
  finishedBids <- dealCards players >>= runBidding
  case finishedBids of
    Successful biddingResults -> do
      trumpsAndTeams <- settleAuction biddingResults
      tricks <- cardPlay (trumps trumpsAndTeams) $ cardPositions biddingResults
      return $ scoreCardPlay trumpsAndTeams (winningBid biddingResults) tricks
    Unsuccessful stalemate -> return $ scoreStalemate stalemate

play :: 
     (Monad f, Ord player)
  => ([player] -> f [(player, [Card])])
  -> ([(player, [Card])] -> f (FinishedBidding player))
  -> (SuccessfulBidding player -> f (TrumpsAndChiefsTeam player)) 
  -> (Trumps -> CardPositions player -> f (Map player [Card]))
  -> EndCondition
  -> [player]
  -> f (Scores player)
play dealCards runBidding settleAuction cardPlay endCondition players = playToTheEnd gameRound' endCondition players
  where
    gameRound' = gameRound dealCards runBidding settleAuction cardPlay (scoreCardPlay (length players)) 




type TopBid = Int

data Bid
  = Pass
  | Raise [Card]
  deriving (Show)


--gameRound ::
--     (Monad f, Ord player)
--  => ([player] ->  f [(player, [Card])])
--  -> ([(player, [Card])] -> f (ResolvedBidding player))
--  -> 
--  -> (TrumpsAndTeams player -> f (CardsWon player))
--  -> [player]
--  -> f (Scores player)
--gameRound dealCards playAuction playCards players = scoreFinishedRound numberOfPlayers <$> finishedRound where
--  numberOfPlayers = length players
--  finishedRound = dealCards players >>= playAuction >>= finishRound
--  finishRound (Unsuccessful stalemate) =
--    return $ FinishedViaStalemate stalemate
--  finishRound (Successful trumpsAndTeams) =
--    FinishedViaCardPlay trumpsAndTeams <$> playCards trumpsAndTeams
