{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module New.PlayerStateTry where

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Lens
import New.Players
import Data.Bifunctor (first)


getPlayerState :: (MonadState (player, players) m) => (player -> Getting s players s) ->  m s
getPlayerState = gets . view . pLens 
      
setPlayerState
  :: MonadState (player, players) m =>  (player -> ASetter' players s) ->  s -> m ()
setPlayerState l = modifyPlayerState l . const 

nextPlayer
  :: (MonadState (player, players) m, Cycling player) => m ()
nextPlayer = modify $ first successor 

choosePlayer 
  :: MonadState (player, players) m => player -> m ()
choosePlayer = modify . first . const 

modifyPlayerState :: MonadState (player, players) m => (player -> ASetter' players s) ->  (s -> s) -> m ()
modifyPlayerState l = modify . over (pLens l)  

gameState
  :: MonadState (p, ps) m => (p -> LensLike' ((,) a) ps s) -> 
     (s -> (a, s)) -> m a
gameState l = state . pLens l  

pLens :: Functor f => (p -> LensLike' f ps s) -> LensLike' f (p, ps) s
pLens l f (p, ps) = (,) p <$> (l p) f ps


