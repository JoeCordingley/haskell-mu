module Game
  (
  ) where

import           AuctionPlay
import           Cards
import           Scoring

--gameRound :: Monad f => f [(player,[Card])] -> ([(player,[Card])] -> f ( FinishedAuction player)) ->  (TrumpsAndTeams player -> f (CardsWon player)) -> f (Scores player)
--gameRound dealCards playAuction playCards = do
--  cards <- dealCards
--  finishedAuction <- playAuction cards
--  finishedRound <- case finishedAuction of
--    Unsuccessful stalemate -> return $ FinishedViaStalemate stalemate
--    Successful trumpsAndTeams -> fmap (\cardsWon -> FinishedViaCardPlay trumpsAndTeams cardsWon) $ playCards trumpsAndTeams
--  return undefined
--gameRound ::
--     Monad f
--  => f [(player, [Card])]
--  -> ([(player, [Card])] -> f (FinishedAuction player))
--  -> (TrumpsAndTeams player -> f (CardsWon player))
--  -> f (FinishedRound player)
--gameRound dealCards playAuction playCards =
--  dealCards >>= playAuction >>= finishRound
--  where
--    finishRound (UnsuccessfulAuction stalemate) =
--      return $ FinishedViaStalemate stalemate
--    finishRound (SuccessfulAuction trumpsAndTeams) =
--      FinishedViaCardPlay trumpsAndTeams <$> playCards trumpsAndTeams
