{-# LANGUAGE FlexibleContexts  #-}

module Mu.SinglePlayer where

import qualified Mu.AI.Random as Random
import Mu.GamePlay
import Mu.GamePlayNPlayers
import Mu.Players
import           Data.Tuple.Homogenous
import qualified Mu.WebsocketInteractions as WS
import           Control.Monad.Random
import     Data.Functor.Bind
import Cards (Score)
import Control.Monad.State.Lazy
import Control.Monad.Except 
import           Servant.Server           (ServerError)
import           Network.WebSockets       (Connection)

--singlePlayerUpdates player = updates 


playMuThreePlayers
  :: (Bind m, MonadError ServerError m, MonadIO m, MonadRandom m) => NOfThree -> Connection -> EndCondition -> m (Tuple3 Score)
playMuThreePlayers player conn = flip evalStateT mempty . playMuThreePlayersWithUpdates updates' dependencies' where
  dependencies' = dependencies exceptThree getThree singularDependencies moveUpdates
  singularDependencies = aAndBs playerDeps aiDeps
  aAndBs a b = Tuple3 $ case player of
    OneOfThree -> (a, b, b)
    TwoOfThree -> (b, a, b)
    ThreeOfThree -> (b, b, a)
  aiDeps = Random.singularDependencies
  playerDeps = WS.singularDependencies threeLens conn player
  moveUpdates = aAndBs playerMoveUpdates aiMoveUpdates
  playerMoveUpdates = WS.moveUpdates conn
  aiMoveUpdates = Random.moveUpdates
  updates' = updates singularUpdates getThree
  singularUpdates = aAndBs playerUpdates aiUpdates
  aiUpdates = Random.singularUpdates
  playerUpdates = WS.singularUpdates threeLens conn player

playMuFourPlayers
  :: (Bind m, MonadError ServerError m, MonadIO m, MonadRandom m) => NOfFour -> Connection -> EndCondition -> m (Tuple4 Score)
playMuFourPlayers player conn = flip evalStateT mempty . playMuFourPlayersWithUpdates updates' dependencies' where
  dependencies' = dependencies exceptFour getFour singularDependencies moveUpdates
  singularDependencies = aAndBs playerDeps aiDeps
  aAndBs a b = Tuple4 $ case player of
    OneOfFour -> (a, b, b, b)
    TwoOfFour -> (b, a, b, b)
    ThreeOfFour -> (b, b, a,b)
    FourOfFour -> (b, b, b, a)
  aiDeps = Random.singularDependencies
  playerDeps = WS.singularDependencies fourLens conn player
  moveUpdates = aAndBs playerMoveUpdates aiMoveUpdates
  playerMoveUpdates = WS.moveUpdates conn
  aiMoveUpdates = Random.moveUpdates
  updates' = updates singularUpdates getFour
  singularUpdates = aAndBs playerUpdates aiUpdates
  aiUpdates = Random.singularUpdates
  playerUpdates = WS.singularUpdates fourLens conn player
