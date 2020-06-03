{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Mu.WebsocketInteractions where

import Cards (Card, Trump(..), ChiefTrump(..))
import           Network.WebSockets            (Connection)
import Mu.BiddingWebsockets (indexList)
import Mu.Auction 
import Data.Aeson
import Websockets.Websockets (receiveJSONOrServerError, sendJSON)
import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Data.Map.Lazy as Map
import           Data.Map.Lazy   (Map)
import           Servant.Server                (ServerError (..), err400)

getMany :: (ToJSON b, MonadIO m, MonadError ServerError m) => Connection -> (Map Int a -> b) -> [a] -> m [a]
getMany conn f as = do
  sendJSON conn $ f map
  is <- receiveJSONOrServerError conn
  traverse lookup is where
    map = indexList as
    lookup i = case Map.lookup i map of
      Nothing -> throwError $ err400 {errBody = "invalid index" }
      Just a -> return a

getOne :: (ToJSON b, MonadIO m, MonadError ServerError m) => Connection -> (Map Int a -> b) -> [a] -> m a
getOne conn f as = do
  sendJSON conn $ f map
  i <- receiveJSONOrServerError conn
  case Map.lookup i map of
    Nothing -> throwError $ err400 {errBody = "invalid index" }
    Just a -> return a
  where
    map = indexList as

data RequestWithPlayer player a = RequestWithPlayer player a
newtype BidRequest a = BidRequest a
newtype ViceTrumpRequest a = ViceTrumpRequest a
newtype ChiefTrumpRequest a = ChiefTrumpRequest a
newtype PartnerRequest a = PartnerRequest a
newtype CardRequest a = CardRequest a

instance (ToJSON player, ToJSON a) => ToJSON (RequestWithPlayer player a) where
  toJSON (RequestWithPlayer player a) = object ["player" .= player, "request" .= a]

instance ToJSON a => ToJSON (BidRequest a) where
  toJSON (BidRequest a) = object ["bid" .= a]

instance ToJSON a => ToJSON (ViceTrumpRequest a) where
  toJSON (ViceTrumpRequest a) = object ["vice-trump" .= a]

instance ToJSON a => ToJSON (ChiefTrumpRequest a) where
  toJSON (ChiefTrumpRequest a) = object ["chief-trump" .= a]

instance ToJSON a => ToJSON (PartnerRequest a) where
  toJSON (PartnerRequest a) = object ["partner" .= a]

instance ToJSON a => ToJSON (CardRequest a) where
  toJSON (CardRequest a) = object ["card" .= a]


getBidSingle :: (ToJSON player, MonadIO m, MonadError ServerError m) => Connection -> player -> [Card] -> m Bid
getBidSingle conn player = fmap toBid . getMany conn (RequestWithPlayer player . BidRequest)

getViceTrumpSingle :: (ToJSON player, MonadIO m, MonadError ServerError m) => Connection -> player -> [Trump] -> m ViceTrump
getViceTrumpSingle conn player = fmap ViceTrump . getOne conn (RequestWithPlayer player . ViceTrumpRequest)

getChiefTrumpSingle :: (ToJSON player, MonadIO m, MonadError ServerError m) => Connection -> player -> [Trump] -> m ChiefTrump
getChiefTrumpSingle conn player = fmap ChiefTrump . getOne conn (RequestWithPlayer player . ChiefTrumpRequest)

getPartnerSingle :: (ToJSON player, MonadIO m, MonadError ServerError m) => Connection -> player -> [player] -> m (Partner player)
getPartnerSingle conn player = fmap Partner . getOne conn (RequestWithPlayer player . PartnerRequest)

getCardSingle :: (ToJSON player, MonadIO m, MonadError ServerError m) => Connection -> player -> [Card] -> m Card
getCardSingle conn player = getOne conn (RequestWithPlayer player . CardRequest)

instance ToJSON Trump where
  toJSON (SuitTrump suit) = toJSON suit
  toJSON (RankTrump rank) = toJSON rank
  toJSON NoTrump = String "no trump"

toBid :: [Card] -> Bid
toBid [] = Pass
toBid cards = Raise cards
