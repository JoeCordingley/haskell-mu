{-# LANGUAGE FlexibleContexts #-}

module Mu.Deal where

import           Cards
import           Control.Monad.Random.Class
import           Control.Monad.State.Lazy
import           Data.List.Split
import           Data.Tuple.Homogenous
import           System.Random.Shuffle
import           TupleInstances
import Data.Functor.Compose
import Mu.Players

deal3 :: (MonadRandom m) => m (Tuple3 [Card])
deal3 = shuffleAndDeal reducedDeck

deal3WithUpdate :: (MonadRandom m) => (NOfThree -> [Card] -> m ()) -> m (Tuple3 [Card])
deal3WithUpdate update = shuffleAndDealWithUpdate update threePlayers reducedDeck

deal4 :: (MonadRandom m) => m (Tuple4 [Card])
deal4 = shuffleAndDeal fullDeck

deal4WithUpdate :: (MonadRandom m) => (NOfFour -> [Card] -> m ()) -> m (Tuple4 [Card])
deal4WithUpdate update = shuffleAndDealWithUpdate update fourPlayers fullDeck

deal5 :: (MonadRandom m) => m (Tuple5 [Card])
deal5 = shuffleAndDeal fullDeck

deal5WithUpdate :: (MonadRandom m) => (NOfFive -> [Card] -> m ()) -> m (Tuple5 [Card])
deal5WithUpdate update = shuffleAndDealWithUpdate update fivePlayers fullDeck

deal6 :: (MonadRandom m) => m (Tuple6 [Card])
deal6 = shuffleAndDeal fullDeck

deal6WithUpdate :: (MonadRandom m) => (NOfSix -> [Card] -> m ()) -> m (Tuple6 [Card])
deal6WithUpdate update = shuffleAndDealWithUpdate update sixPlayers fullDeck

shuffleAndDeal ::
     (MonadRandom m, Traversable t, Monoid (t [c])) => [c] -> m (t [c])
shuffleAndDeal = fmap deal . shuffleM

shuffleAndDealWithUpdate ::
     (MonadRandom m, Traversable t) => (player -> [c] -> m ()) -> t player ->  [c] -> m (t [c])
shuffleAndDealWithUpdate update players = dealAndUpdate update players <=< shuffleM

deal :: (Traversable t, Monoid (t [a])) => [a] -> t [a]
deal = evalState $ deal' mempty
  where
    deal' hand = do
      remaining <- get
      if null remaining then return hand else dealRound hand >>= deal'

dealAndUpdate :: (Traversable t,  Monad m) => (player -> [card] -> m ()) -> t player -> [card] -> m (t [card])
dealAndUpdate update players = traverse update' . dealToPlayers players where
  update' (player, cards) = cards <$ update player cards

dealToPlayers :: Traversable t  => t player -> [a] -> t (player, [a])
dealToPlayers players = evalState  . deal' $ fmap emptyHand players where
  emptyHand player = (player, [])
  deal' hand = do
    remaining <- get
    if null remaining then return hand else dealRoundToPlayers hand >>= deal'

dealRoundToPlayers :: (MonadState [a] m, Traversable t) => t (player, [a]) -> m (t (player, [a]))
dealRoundToPlayers = fmap getCompose . dealRound . Compose

dealRound :: (MonadState [a] m, Traversable t) => t [a] -> m (t [a])
dealRound = traverse (state . dealOne)
  where
    dealOne hand (card:deck) = (card : hand, deck)
    dealOne hand []          = (hand, [])
