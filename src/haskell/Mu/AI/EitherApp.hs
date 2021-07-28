module Mu.AI.EitherApp where

data E l r = L l | R r

instance Functor ( E l ) where
  fmap f (R r) = R ( f r )

--instance Applicative (E l) where 
--  pure r = R r
--  (R fab) <*> (R a) = R (fab a)
--  _ <*> (L l) = L l
--  (L l) <*> _ = L l


instance Semigroup l => Applicative (E l) where 
  pure r = R r
  (R fab) <*> (R a) = R (fab a)
  (L l1) <*> (L l2) = L (l1 <> l2)
  _ <*> (L l) = L l
  (L l) <*> _ = L l
