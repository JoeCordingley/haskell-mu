{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Mu.BiddingWebsockets where

import           Control.Monad.IO.Class  (MonadIO)
import           Protolude

import           Cards                   (Card (..), Suit (..))
import           Control.Lens            hiding ((.=))
import           Control.Monad.Except    (MonadError, liftEither)
import           Control.Monad.Fail      (fail)
import qualified Control.Monad.List      as ML
import           Data.Aeson
import qualified Data.Aeson              as JSON
import qualified Data.List.Index         as List
import qualified Data.Map.Lazy           as Map
import           Data.Semigroup.Foldable
import           Data.Text               (pack)
import           Data.Tuple.Homogenous
import           Data.Validation
import           Mu.Auction              (Bid (..), CardPositions,
                                          FinishedBidding, playAuctionAndRecord)
import           Mu.Players
import           Network.WebSockets      (Connection)
import           Servant.Server          (ServerError (..), err400)
import           Websockets.Websockets   (receiveJSONOrServerError, sendJSON)

data BidRequest =
  BidRequest [IndexedValue Card]

instance ToJSON Suit where
  toJSON Red    = JSON.String "Red"
  toJSON Black  = JSON.String "Black"
  toJSON Blue   = JSON.String "Blue"
  toJSON Yellow = JSON.String "Yellow"
  toJSON Green  = JSON.String "Green"

instance ToJSON Card where
  toJSON (Card suit rank) = JSON.object ["suit" .= suit, "rank" .= rank]

data IndexedValue a =
  IndexedValue
    { index :: Int
    , value :: a
    }

instance ToJSON a => ToJSON (IndexedValue a) where
  toJSON (IndexedValue i a) = JSON.object ["index" .= i, "value" .= a]

instance ToJSON BidRequest where
  toJSON (BidRequest cards) =
    JSON.object ["bid-request" .= JSON.object ["from-cards" .= cards]]

data BidResponse
  = PassResponse
  | RaiseResponse [Int]

instance FromJSON BidResponse where
  parseJSON =
    withObject "BidResponse" $ \o ->
      do t <- o .: "bid-response"
         case (t :: Text) of
           "pass" -> pure PassResponse
           _      -> fail "expected a 'pass'"
     <|> do
        t <- o .: "bid-response"
        is <- t .: "raise"
        pure (RaiseResponse is)

indexList :: Foldable t => t a -> Map Int a
indexList = Map.fromList . List.indexed . toList

indexed' :: MonadError Text m => (Map Int a -> m Int) -> [a] -> m a
indexed' f as = do
  i <- f map
  case Map.lookup i map of
    Just a -> return a
    Nothing -> throwError $ "no " <> show i
  where
    map = indexList as

indexedMany :: MonadError Text m => (Map Int a -> m [Int]) -> [a] -> m [a]
indexedMany f as = do
  is <- f map
  traverse lookup is 
  where
    lookup i = case Map.lookup i map of
      Just a -> return a
      Nothing -> throwError $ "no " <> show i
    map = indexList as
  

getBidWS ::
     (MonadIO m, MonadError ServerError m)
  => s
  -> (n -> Getting Connection s Connection)
  -> n
  -> [Card]
  -> m Bid
getBidWS cs l n cards = do
  sendJSON conn bidRequest
  response <- receiveJSONOrServerError conn
  case response of
    PassResponse -> pure Pass
    RaiseResponse keys ->
      fmap Raise . liftEither . first unknownKey . values map $ keys
  where
    conn = view (l n) cs
    indexed = List.indexed cards
    bidRequest = BidRequest $ fmap (uncurry IndexedValue) indexed
    map = Map.fromList indexed

--getBidThreeWS :: (MonadIO m, MonadError ServerError m) => Tuple3 Connection -> NOfThree -> [Card] -> m Bid
--getBidThreeWS cs = getBidWS cs threeLens
--
--getBidFourWS :: (MonadIO m, MonadError ServerError m) => Tuple4 Connection -> NOfFour -> [Card] -> m Bid
--getBidFourWS cs = getBidWS cs fourLens
--
--getBidFiveWS :: (MonadIO m, MonadError ServerError m) => Tuple5 Connection -> NOfFive -> [Card] -> m Bid
--getBidFiveWS cs = getBidWS cs fiveLens
--
--getBidSixWS :: (MonadIO m, MonadError ServerError m) => Tuple6 Connection -> NOfSix -> [Card] -> m Bid
--getBidSixWS cs = getBidWS cs sixLens
biddingWS ::
     ( Foldable1 players
     , Traversable players
     , Eq player
     , MonadIO m
     , MonadError ServerError m
     , Monoid (players [Card])
     , Cycling player
     , Applicative players
     )
  => players Connection
  -> players player
  -> (forall c. player -> Lens' (players c) c)
  -> player
  -> players [Card]
  -> m (FinishedBidding players player)
biddingWS cs players l = playAuctionAndRecord players l (getBidWS cs l)

biddingThreeWS ::
     (MonadIO m, MonadError ServerError m)
  => Tuple3 Connection
  -> NOfThree
  -> Tuple3 [Card]
  -> m (FinishedBidding Tuple3 NOfThree)
biddingThreeWS cs = biddingWS cs threePlayers threeLens

biddingFourWS ::
     (MonadIO m, MonadError ServerError m)
  => Tuple4 Connection
  -> NOfFour
  -> Tuple4 [Card]
  -> m (FinishedBidding Tuple4 NOfFour)
biddingFourWS cs = biddingWS cs fourPlayers fourLens

biddingFiveWS ::
     (MonadIO m, MonadError ServerError m)
  => Tuple5 Connection
  -> NOfFive
  -> Tuple5 [Card]
  -> m (FinishedBidding Tuple5 NOfFive)
biddingFiveWS cs = biddingWS cs fivePlayers fiveLens

biddingSixWS ::
     (MonadIO m, MonadError ServerError m)
  => Tuple6 Connection
  -> NOfSix
  -> Tuple6 [Card]
  -> m (FinishedBidding Tuple6 NOfSix)
biddingSixWS cs = biddingWS cs sixPlayers sixLens

values :: Ord k => Map k v -> [k] -> Either (NonEmpty k) [v]
values m = toEither . traverse get'
  where
    get' k = validationNel . maybeToEither k $ Map.lookup k m

unknownKey :: Show k => NonEmpty k -> ServerError
unknownKey keys = err400 {errBody = "invalid keys: " <> show keys}

