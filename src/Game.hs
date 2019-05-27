module Game
  (
  ) where

import           AuctionPlay
import           Cards
import           Scoring

data EndCondition
  = NumberOfRounds Int
  | PointsTarget Int

--gameRound :: Monad f => f [(player,[Card])] -> ([(player,[Card])] -> f ( FinishedAuction player)) ->  (TrumpsAndTeams player -> f (CardsWon player)) -> f (Scores player)
--gameRound dealCards playAuction playCards = do
--  cards <- dealCards
--  finishedAuction <- playAuction cards
--  finishedRound <- case finishedAuction of
--    Unsuccessful stalemate -> return $ FinishedViaStalemate stalemate
--    Successful trumpsAndTeams -> fmap (\cardsWon -> FinishedViaCardPlay trumpsAndTeams cardsWon) $ playCards trumpsAndTeams
--  return undefined
gameRound ::
     Monad f
  => f [(player, [Card])]
  -> ([(player, [Card])] -> f (FinishedAuction player))
  -> (TrumpsAndTeams player -> f (CardsWon player))
  -> f (FinishedRound player)
gameRound dealCards playAuction playCards =
  dealCards >>= playAuction >>= finishRound
  where
    finishRound (Unsuccessful stalemate) =
      return $ FinishedViaStalemate stalemate
    finishRound (Successful trumpsAndTeams) =
      FinishedViaCardPlay trumpsAndTeams <$> playCards trumpsAndTeams