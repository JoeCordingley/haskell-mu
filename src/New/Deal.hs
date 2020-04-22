module New.Deal where

import System.Random.Shuffle
import Control.Monad.Random.Class
import Cards
import           Data.List.Split
import Data.Tuple.Homogenous



instance Semigroup m => Semigroup (Tuple3 m)
  where
    Tuple3 (l1, l2, l3) <> Tuple3 (r1, r2, r3) = Tuple3 (l1 <> r1, l2 <> r2, l3 <> r3)

instance Monoid m => Monoid (Tuple3 m)
  where
    mempty = Tuple3 (mempty, mempty, mempty)

instance Semigroup m => Semigroup (Tuple4 m)
  where
    Tuple4 (l1, l2, l3, l4) <> Tuple4 (r1, r2, r3, r4) = Tuple4 (l1 <> r1, l2 <> r2, l3 <> r3, l4 <> r4)

instance Monoid m => Monoid (Tuple4 m)
  where
    mempty = Tuple4 (mempty, mempty, mempty, mempty)

instance Semigroup m => Semigroup (Tuple5 m)
  where
    Tuple5 (l1, l2, l3, l4, l5) <> Tuple5 (r1, r2, r3, r4, r5) = Tuple5 (l1 <> r1, l2 <> r2, l3 <> r3, l4 <> r4, l5 <>r5)

instance Monoid m => Monoid (Tuple5 m)
  where
    mempty = Tuple5 (mempty, mempty, mempty, mempty, mempty)

instance Semigroup m => Semigroup (Tuple6 m)
  where
    Tuple6 (l1, l2, l3, l4, l5, l6) <> Tuple6 (r1, r2, r3, r4, r5, r6) = Tuple6 (l1 <> r1, l2 <> r2, l3 <> r3, l4 <> r4, l5 <>r5, l6 <> r6)

instance Monoid m => Monoid (Tuple6 m)
  where
    mempty = Tuple6 (mempty, mempty, mempty, mempty, mempty, mempty)

deal3 :: (MonadRandom m) => m (Tuple3 [Card])
deal3 = (foldMap dealRound . chunksOf 3) <$> shuffleM reducedDeck
  where 
    dealRound [a,b,c] = Tuple3( [a], [b], [c])

deal4 :: (MonadRandom m) => m (Tuple4 [Card])
deal4 = (foldMap dealRound . chunksOf 4) <$> shuffleM reducedDeck
  where 
    dealRound [a,b,c, d] = Tuple4 ([a], [b],[c],[d])

deal5 :: (MonadRandom m) => m (Tuple5 [Card])
deal5 = (foldMap dealRound . chunksOf 5) <$> shuffleM reducedDeck
  where 
    dealRound [a, b, c, d, e] = Tuple5 ([a], [b],[c],[d], [e])

deal6 :: (MonadRandom m) => m (Tuple6 [Card])
deal6 = (foldMap dealRound . chunksOf 6) <$> shuffleM reducedDeck
  where 
    dealRound [a,b,c, d, e, f] = Tuple6([a], [b],[c],[d], [e], [f])
