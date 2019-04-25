module OneMoreTry where

import Control.Monad.State.Lazy
import qualified Data.Map.Lazy as Map

insert :: Eq k => k -> v -> (k -> v) -> k -> v
insert k' v m k = if k == k' then v else m k

insertWith ::  Eq k => (v -> v -> v) -> k -> v -> (k -> v) -> k -> v
insertWith f k' v m k = if k == k' then f v $ m k else m k


data MemoizedFunction k v = MemoizedFunction (Map.Map k v) (k -> v)

--apply :: Ord k => k -> State (MemoizedFunction k v) v
--apply k = do
--  MemoizedFunction map f <- get
--  maybeV <- return $ Map.lookup k map
--  case maybeV of
--    Just v -> return v
--    Nothing -> do
--      v <- return $ f k
--      map' <- return $ Map.insert k v map
--      put $ MemoizedFunction map' f
--      return v



insertWithStateful :: Ord k => (v -> v -> v) -> k -> v -> State (MemoizedFunction k v) ()
insertWithStateful combine k v = undefined 

--insertWithStateful :: Ord k => (v -> v -> v) -> k -> v -> State (MemoizedFunction k v) ()
--insertWithStateful combine k v = do
--  MemoizedFunction map f <- get
--  put $ MemoizedFunction (insertWith combine map)



