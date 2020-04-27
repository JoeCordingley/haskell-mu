module New.TupleInstances where

import Data.Tuple.Homogenous
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Functor.Apply

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

instance Foldable1 Tuple3 where
  foldMap1 f (Tuple3 (a1, a2, a3)) = (f a1) <> (f a2) <> (f a3)

instance Traversable1 Tuple3 where
  traverse1 f (Tuple3 (a1, a2, a3)) = fmap Tuple3 $ (,,) <$> f a1 <.> f a2 <.> f a3
