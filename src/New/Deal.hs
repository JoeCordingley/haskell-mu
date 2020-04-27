{-# LANGUAGE FlexibleContexts #-}
module New.Deal where

import System.Random.Shuffle
import Control.Monad.Random.Class
import Cards
import           Data.List.Split
import Data.Tuple.Homogenous
import New.TupleInstances
import Control.Monad.State.Lazy

deal3 :: (MonadRandom m) => m (Tuple3 [Card])
deal3 = shuffleAndDeal reducedDeck

deal4 :: (MonadRandom m) => m (Tuple4 [Card])
deal4 = shuffleAndDeal fullDeck

deal5 :: (MonadRandom m) => m (Tuple5 [Card])
deal5 = shuffleAndDeal fullDeck

deal6 :: (MonadRandom m) => m (Tuple6 [Card])
deal6 = shuffleAndDeal fullDeck

shuffleAndDeal :: (MonadRandom m, Traversable t, Monoid (t [c])) => [c] -> m (t [c])
shuffleAndDeal = fmap deal . shuffleM 

deal :: (Traversable t, Monoid (t [a])) => [a] -> t [a]
deal = evalState $ deal' mempty where
  deal' hand = do
    remaining <- get
    case remaining of
      [] -> return hand
      _ -> dealRound hand >>= deal'

dealRound :: (MonadState [a] m, Traversable t) => t [a] -> m (t [a])
dealRound = traverse (state . dealOne) where
  dealOne hand (card:deck) = (card:hand, deck)
  dealOne hand [] = (hand, [])

