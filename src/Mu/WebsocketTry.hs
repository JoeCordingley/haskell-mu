{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Mu.WebsocketTry where

import           Control.Monad.Except
import           Control.Monad.State.Lazy
import           Mu.GamePlay              (EndCondition (..))
import           Network.Wai.Handler.Warp (run)
import           Network.WebSockets       (Connection)
import           Servant
import           Servant.API.WebSocket    (WebSocket)
import           Servant.Server           (ServerError)
import           Websockets.Websockets    (keepAlive)
import qualified Mu.SinglePlayer as SinglePlayer
import Mu.Players

threePlayerTwoRounds ::
     (MonadIO m, MonadError ServerError m) => Connection -> m ()
threePlayerTwoRounds conn = keepAlive conn play
  where
    play = do
      score <- SinglePlayer.playMuThreePlayers OneOfThree conn (SetNumberOfRounds 2)
      return ()

fourPlayerTwoRounds ::
     (MonadIO m, MonadError ServerError m) => Connection -> m ()
fourPlayerTwoRounds conn = keepAlive conn play
  where
    play = do
      score <- SinglePlayer.playMuFourPlayers OneOfFour conn (SetNumberOfRounds 2)
      return ()

type TestApi = "trythree" :> WebSocket :<|> "tryFour" :> WebSocket

api :: Proxy TestApi
api = Proxy

main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:8080"
  run 8080 app

app :: Application
app = serve api (threePlayerTwoRounds :<|> fourPlayerTwoRounds)

