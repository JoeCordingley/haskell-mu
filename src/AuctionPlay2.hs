{-# LANGUAGE MultiParamTypeClasses #-}
module AuctionPlay2 (bidding, GetBid(..), Bid(..)) where

import Data.List.NonEmpty (NonEmpty)
import Cards

data Bid = Pass | Raise (NonEmpty Card)

class Monad m => GetBid m player where
  getBid :: Int -> player -> [Card] -> m Bid

data PlayerRaise player = PlayerRaise player (NonEmpty Card)

bidding :: (Ord player, GetBid f player ) => [(player, [Card])] -> f [PlayerRaise player]
bidding players = bidding' playerSequence 0 where
  playerSequence = cycle players
  bidding' = undefined
  numberOfPlayers = length players
