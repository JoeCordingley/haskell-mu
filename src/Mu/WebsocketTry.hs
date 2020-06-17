{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeOperators     #-}

module Mu.WebsocketTry where

import Control.Monad.Except
import           Network.WebSockets            (Connection)
import           Servant.Server                (ServerError)
import Websockets.Websockets (keepAlive)
import Mu.WebsocketInteractions (playMuSingleConnectionThreePlayers)
import Mu.GamePlay (EndCondition(..))
import           Servant.API.WebSocket         (WebSocket)
import           Servant                       
import           Network.Wai.Handler.Warp      (run)
import Control.Monad.State.Lazy

threePlayerTwoRounds ::
    (MonadIO m, MonadError ServerError m) 
    => Connection
    -> m ()
threePlayerTwoRounds conn = keepAlive conn play where
  play = do
    score <- playMuSingleConnectionThreePlayers conn (SetNumberOfRounds 2)
    return ()

type TestApi = "trythree" :> WebSocket

api :: Proxy TestApi
api = Proxy

main :: IO ()
main = do
  putStrLn "Starting server on http://localhost:8080"
  run 8080 app 

app ::  Application
app = serve api threePlayerTwoRounds 
