{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}

module Websockets.Websockets where

import           Control.Applicative           (Alternative, (<|>))
import           Control.Concurrent            (threadDelay)
import           Control.Concurrent            (MVar, modifyMVar, modifyMVar_,
                                                newMVar, readMVar)
import qualified Control.Concurrent.Async      as Async
import           Control.Lens                  hiding ((.=))
import           Control.Monad                 (forever, mzero, void, when,
                                                (<=<))
import           Control.Monad.Except          (ExceptT, MonadError, liftEither,
                                                runExceptT, throwError)
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Control.Monad.Reader          (MonadReader, ask)
import           Control.Monad.State.Lazy      (MonadState, runState, state)
import           Data.Aeson
import qualified Data.ByteString.Lazy.UTF8     as B
import           Data.Composition              ((.*), (.**))
import           Data.Foldable                 (forM_, traverse_)
import qualified Data.HashMap.Strict           as HM
import           Data.Map.Lazy                 (Map)
import qualified Data.Map.Lazy                 as Map
import           Data.Maybe                    (catMaybes, fromMaybe, isJust,
                                                isNothing)
import           Data.Semigroup                (Any (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Tuple.Homogenous
import           GHC.Generics
import           Network.Wai                   (Application)
import           Network.Wai.Handler.Warp      (run)
import           Network.WebSockets            (Connection, receiveData,
                                                receiveDataMessage,
                                                sendTextData, withPingThread)
import           Network.WebSockets.Connection (pingThread)
import           Servant                       ((:>), Capture, Proxy (..),
                                                Server, serve)
import           Servant.API                   (FromHttpApiData, parseUrlPiece)
import           Servant.API.WebSocket         (WebSocket)
import           Servant.Server                (ServerError (..), err400)

newtype Name =
  Name String
  deriving (Eq, Ord, Generic)

instance ToJSON Name

instance FromHttpApiData Name where
  parseUrlPiece = fmap Name . parseUrlPiece

newtype PlayerNumberInput =
  PlayerNumberInput String

type WebSocketApi = "join" :> Capture "name" Name :> WebSocket

api :: Proxy WebSocketApi
api = Proxy

server ::
     (MonadIO m, MonadError ServerError m)
  => MVar State
  -> Name
  -> Connection
  -> m ()
server stateRef name conn = keepAlive conn commmunication
  where
    commmunication = do
      addConn'
      broadcastPlayers'
      PlayerNumberRequest n <- receiveJSONOrServerError conn
      updated <- updatePlayer' n name
      traverse_ (broadcastUpdateToUsers n) updated
      wait conn
    broadcastPlayers' =
      sendJSON conn =<< CurrentPlayers . statePlayers <$>
      liftIO (readMVar stateRef)
    addConn' =
      liftIO $ modifyMVar_ stateRef $ pure .
      over stateUserLens (Map.insert name conn)
    updatePlayer' n name =
      liftIO $ modifyMVar stateRef $ pure . updatePlayer n name
    broadcastUpdateToUser t n conn =
      sendJSON conn (NewPlayer n name) *>
      when (isJust $ sequence t) (sendJSON conn StartMsg)
    broadcastUpdateToUsers n (State users t) =
      traverse_ (broadcastUpdateToUser t n) users

keepAlive
  :: (MonadError e m, MonadIO m) => Connection
     -> ExceptT e IO c
     -> m c
keepAlive conn =
  liftEither <=< liftIO . withPingThread conn 30 (pure ()) . runExceptT

data StartMsg =
  StartMsg
  deriving (Generic)

instance ToJSON StartMsg

data NewPlayer =
  NewPlayer
    { newPlayerNumber :: NOfThree
    , newPlayerName   :: Name
    }

instance ToJSON NewPlayer where
  toJSON (NewPlayer number name) =
    object ["newPlayer" .= object ["number" .= number, "name" .= name]]

receiveJSONOrServerError ::
     (MonadIO m, MonadError ServerError m, FromJSON a) => Connection -> m a
receiveJSONOrServerError = stringError <=< runExceptT . receiveJSON

receiveJSON :: (MonadIO m, MonadError String m, FromJSON a) => Connection -> m a
receiveJSON = liftEither . eitherDecode <=< liftIO . receiveData

leftMap :: (MonadError e m, MonadError f n) => (e -> f) -> m a -> n a
leftMap f = undefined

sendJSON :: (ToJSON a, MonadIO m) => Connection -> a -> m ()
sendJSON conn a = liftIO $ sendTextData conn (encode a)

stringError :: (MonadError ServerError m) => Either String a -> m a
stringError (Left s)  = throwError err400 {errBody = B.fromString s}
stringError (Right a) = pure a

playerNumber' :: Text -> Maybe NOfThree
playerNumber' t =
  case T.unpack t of
    "1" -> return OneOfThree
    "2" -> return TwoOfThree
    "3" -> return ThreeOfThree
    _   -> Nothing

wait :: MonadIO m => Connection -> m ()
wait conn = do
  liftIO $ receiveDataMessage conn
  wait conn

updated :: (a -> Maybe a) -> a -> (a, Maybe a)
updated f a = (fromMaybe a (f a), f a)

data PlayerNumberRequest =
  PlayerNumberRequest
    { playerNumber :: NOfThree
    }
  deriving (Generic)

instance FromJSON PlayerNumberRequest

instance FromJSON NOfThree where
  parseJSON (Number 1) = pure OneOfThree
  parseJSON (Number 2) = pure TwoOfThree
  parseJSON (Number 3) = pure ThreeOfThree
  parseJSON _          = mzero

instance ToJSON NOfThree where
  toJSON OneOfThree   = Number 1
  toJSON TwoOfThree   = Number 2
  toJSON ThreeOfThree = Number 3

updatePlayer :: NOfThree -> Name -> State -> (State, Maybe State)
updatePlayer n = updated . updatePlayer'
  where
    updatePlayer' = statePlayersLens . tIso . l . f
    l =
      case n of
        OneOfThree   -> _1
        TwoOfThree   -> _2
        ThreeOfThree -> _3
    f name Nothing  = Just (Just name)
    f name (Just _) = Nothing

data State =
  State
    { stateUsers   :: Map Name Connection
    , statePlayers :: Tuple3 (Maybe Name)
    }

type Players = Tuple3 (Maybe Name)

data CurrentPlayers =
  CurrentPlayers Players

instance ToJSON CurrentPlayers where
  toJSON (CurrentPlayers (Tuple3 (playerOne, playerTwo, playerThree))) =
    object
      [ "currentPlayers" .=
        object
          [ "playerOne" .= playerOne
          , "playerTwo" .= playerTwo
          , "playerThree" .= playerThree
          ]
      ]

data PlayerNames =
  PlayerNames
    { playerOne   :: Maybe Name
    , playerTwo   :: Maybe Name
    , playerThree :: Maybe Name
    }
  deriving (Generic)

instance ToJSON PlayerNames

statePlayersLens :: Lens' State Players
statePlayersLens f s = fmap setPlayers . f $ statePlayers s
  where
    setPlayers u = s {statePlayers = u}

stateUserLens :: Lens' State (Map Name Connection)
stateUserLens f s = fmap setUsers . f $ stateUsers s
  where
    setUsers u = s {stateUsers = u}

initialState :: State
initialState = State Map.empty emptyPlayers

startApp :: IO ()
startApp = do
  state <- newMVar initialState
  putStrLn "Starting server on http://localhost:8080"
  run 8080 (app state)

app :: MVar State -> Application
app state = serve api (server state)

main :: IO ()
main = startApp

emptyPlayers = Tuple3 (Nothing, Nothing, Nothing)

data NOfThree
  = OneOfThree
  | TwoOfThree
  | ThreeOfThree

tIso :: Iso' (Tuple3 a) (a, a, a)
tIso = iso untuple3 Tuple3

broadcast :: (MonadIO m, Foldable f) => Text -> f Connection -> m ()
broadcast = liftIO .* foldMap . flip sendTextData

broadcastPlayer ::
     (MonadIO m, Foldable f) => NOfThree -> Name -> f Connection -> m ()
broadcastPlayer n (Name name) = broadcast $ T.pack (show n <> ", " <> name)

broadcastPlayers ::
     (MonadIO m, Foldable f) => Tuple3 Name -> f Connection -> m ()
broadcastPlayers players = undefined

instance Show NOfThree where
  show OneOfThree   = "1"
  show TwoOfThree   = "2"
  show ThreeOfThree = "3"

