{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Mu.WebsocketInteractions where

import           Cards                    (Card, Score, Trump (..))
import           Control.Lens             hiding ((.=))
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Control.Monad.State.Lazy
import           Data.Aeson
import           Data.Functor.Bind
import           Data.Functor.Compose
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map
import           Data.Tuple.Homogenous
import           Mu.Auction
import           Mu.CardPlay              (CardPosition (..), PlayableCard,
                                           WinnerOfTrick (..))
import           Mu.GamePlay              (Dependencies (..), EndCondition,
                                           ScoreUpdate (..), Updates (..))
import           Mu.GamePlayNPlayers      (playMuFivePlayersWithUpdates,
                                           playMuFourPlayersWithUpdates,
                                           playMuSixPlayersWithUpdates,
                                           playMuThreePlayersWithUpdates)
import           Mu.Players               (fiveLens, fourLens, sixLens,
                                           threeLens)
import           Network.WebSockets       (Connection)
import           Servant.Server           (ServerError (..), err400)
import           Util                     (indexList, passThrough)
import           Websockets.Websockets    (receiveJSONOrServerError, sendJSON)

getMany ::
     (ToJSON b, MonadIO m, MonadError ServerError m)
  => Connection
  -> (Map Int a -> b)
  -> [a]
  -> m [a]
getMany conn f as = do
  sendJSON conn $ f map
  is <- receiveJSONOrServerError conn
  traverse lookup is
  where
    map = indexList as
    lookup i =
      case Map.lookup i map of
        Nothing -> throwError $ err400 {errBody = "invalid index"}
        Just a  -> return a

getOne ::
     (ToJSON b, MonadIO m, MonadError ServerError m, Foldable t)
  => Connection
  -> (Map Int a -> b)
  -> t a
  -> m a
getOne conn f as = do
  sendJSON conn $ f map
  i <- receiveJSONOrServerError conn
  case Map.lookup i map of
    Nothing -> throwError $ err400 {errBody = "invalid index"}
    Just a  -> return a
  where
    map = indexList as

data RequestWithPlayer player a =
  RequestWithPlayer player a

newtype BidRequest a =
  BidRequest a

newtype ViceTrumpRequest a =
  ViceTrumpRequest a

newtype ChiefTrumpRequest a =
  ChiefTrumpRequest a

newtype PartnerRequest a =
  PartnerRequest a

newtype CardRequest a =
  CardRequest a

newtype DealUpdate a =
  DealUpdate a

newtype BidResultUpdate a =
  BidResultUpdate a

newtype TrickWinnerUpdate a =
  TrickWinnerUpdate a

data MaxAndCards max cards =
  MaxAndCards
    { max   :: max
    , cards :: cards
    }

instance ToJSON MaxRaise where
  toJSON (MaxRaise max) = object ["max" .= max]

instance (ToJSON max, ToJSON cards) => ToJSON (MaxAndCards max cards) where
  toJSON (MaxAndCards max cards) = object ["max" .= max, "cards" .= cards]

instance (ToJSON player, ToJSON a) => ToJSON (RequestWithPlayer player a) where
  toJSON (RequestWithPlayer player a) =
    object ["player" .= player, "request" .= a]

instance (ToJSON a) => ToJSON (BidRequest a) where
  toJSON (BidRequest a) = object ["bid" .= a]

instance ToJSON a => ToJSON (ViceTrumpRequest a) where
  toJSON (ViceTrumpRequest a) = object ["vice-trump" .= a]

instance ToJSON a => ToJSON (ChiefTrumpRequest a) where
  toJSON (ChiefTrumpRequest a) = object ["chief-trump" .= a]

instance ToJSON a => ToJSON (PartnerRequest a) where
  toJSON (PartnerRequest a) = object ["partner" .= a]

instance ToJSON a => ToJSON (CardRequest a) where
  toJSON (CardRequest a) = object ["card" .= a]

instance ToJSON a => ToJSON (DealUpdate a) where
  toJSON (DealUpdate a) = object ["deal" .= a]

instance ToJSON a => ToJSON (BidResultUpdate a) where
  toJSON (BidResultUpdate a) = object ["bid-result" .= a]

instance ToJSON a => ToJSON (TrickWinnerUpdate a) where
  toJSON (TrickWinnerUpdate a) = object ["trick winner" .= a]

instance ToJSON player => ToJSON (BiddingResult player) where
  toJSON (SuccessfulBiddingResult w)   = object ["successful" .= w]
  toJSON (UnsuccessfulBiddingResult s) = object ["unsuccessful" .= s]

instance ToJSON player => ToJSON (WinnersAndTopBid player) where
  toJSON (WinnersAndTopBid (Chief chief) vice (CardsBid bid)) =
    object ["chief" .= chief, "vice" .= vice, "bid" .= bid]

instance ToJSON player => ToJSON (Stalemate player) where
  toJSON EklatNoPoints = String "all pass"
  toJSON Eklat {topBid = CardsBid bid, atFault, affected} =
    object
      ["bid" .= bid, "offending player" .= atFault, "tied players" .= affected]

instance ToJSON player => ToJSON (Vice player) where
  toJSON (Vice player) = toJSON player

instance ToJSON player => ToJSON (WinnerOfTrick player) where
  toJSON (WinnerOfTrick player) = toJSON player

instance ToJSON scores => ToJSON (ScoreUpdate scores) where
  toJSON ScoreUpdate {roundScores, runningScores} =
    object
      [ "scores" .=
        object
          ["round scores" .= roundScores, "running scores" .= runningScores]
      ]

instance ToJSON CardPosition where
  toJSON InHand  = "in hand"
  toJSON OnTable = "on table"

getBidSingle ::
     (ToJSON player, MonadIO m, MonadError ServerError m)
  => Connection
  -> player
  -> MaxRaise
  -> [Card]
  -> m Bid
getBidSingle conn player (MaxRaise max) =
  fmap toBid .
  getMany conn (RequestWithPlayer player . BidRequest . MaxAndCards max)

getViceTrumpSingle ::
     (ToJSON player, MonadIO m, MonadError ServerError m)
  => Connection
  -> Vice player
  -> [Trump]
  -> m ViceTrump
getViceTrumpSingle conn (Vice player) =
  fmap ViceTrump . getOne conn (RequestWithPlayer player . ViceTrumpRequest)

getChiefTrumpSingle ::
     (ToJSON player, MonadIO m, MonadError ServerError m)
  => Connection
  -> Chief player
  -> [Trump]
  -> m ChiefTrump
getChiefTrumpSingle conn (Chief player) =
  fmap ChiefTrump . getOne conn (RequestWithPlayer player . ChiefTrumpRequest)

getPartnerSingle ::
     (ToJSON player, MonadIO m, MonadError ServerError m, Foldable players)
  => Connection
  -> Chief player
  -> players player
  -> m (Partner player)
getPartnerSingle conn (Chief chief) =
  fmap Partner . getOne conn (RequestWithPlayer chief . PartnerRequest)

getCardSingle ::
     (ToJSON player, MonadIO m, MonadError ServerError m)
  => Connection
  -> player
  -> [PlayableCard]
  -> m PlayableCard
getCardSingle conn player = getOne conn (RequestWithPlayer player . CardRequest)

dealUpdateSingle ::
     (ToJSON player, MonadIO m, MonadError ServerError m)
  => Connection
  -> player
  -> [Card]
  -> m ()
dealUpdateSingle conn player =
  sendJSON conn . RequestWithPlayer player . DealUpdate

getBidPreIndexed ::
     ( MonadIO m
     , MonadError ServerError m
     , MonadState playerCards m
     , FromJSON a
     , Ord a
     )
  => (player -> LensLike' (Compose m ((,) Card)) playerCards (Map a PlayableCard))
  -> Connection
  -> player
  -> MaxRaise
  -> m Bid
getBidPreIndexed l conn player maxRaise = do
  sendJSON conn (BidRequest maxRaise)
  is <- receiveJSONOrServerError conn
  fmap toBid $ traverse lookup is
  where
    lookup i = passThrough (l player . at i) moveToTable
    moveToTable (Just (card, InHand)) = return (card, Just (card, OnTable))
    moveToTable _ = throwError $ err400 {errBody = "invalid index"}

getCardPreIndexed ::
     (MonadIO m, MonadError ServerError m, MonadState playerCards m, Eq card)
  => (player -> Lens' playerCards (Map Int card))
  -> Connection
  -> player
  -> [card]
  -> m card
getCardPreIndexed l conn player cards = do
  map <- uses (l player) $ Map.filter $ flip elem cards
  sendJSON conn (CardRequest $ Map.keys map)
  i <- receiveJSONOrServerError conn
  case Map.lookup i map of
    Just c  -> return c
    Nothing -> throwError $ err400 {errBody = "invalid index"}

dealUpdatePreIndexed ::
     (MonadState playerCards m, MonadIO m)
  => (player -> ASetter' playerCards (Map Int PlayableCard))
  -> Connection
  -> player
  -> [Card]
  -> m ()
dealUpdatePreIndexed l conn player cards = do
  assign (l player) $ fmap (, InHand) map
  sendJSON conn $ DealUpdate map
  where
    map = indexList cards

biddingResultUpdateWS ::
     (ToJSON player, MonadIO m, MonadError ServerError m)
  => Connection
  -> BiddingResult player
  -> m ()
biddingResultUpdateWS conn = sendJSON conn . BidResultUpdate

trickWinnerUpdateWS ::
     (ToJSON player, MonadIO m, MonadError ServerError m)
  => Connection
  -> WinnerOfTrick player
  -> m ()
trickWinnerUpdateWS conn = sendJSON conn . TrickWinnerUpdate

scoresUpdateWS ::
     (ToJSON scores, MonadIO m, MonadError ServerError m)
  => Connection
  -> ScoreUpdate scores
  -> m ()
scoresUpdateWS conn = sendJSON conn

singleConnectionDeps ::
     (MonadIO f, MonadError ServerError f, ToJSON player, Foldable players)
  => Connection
  -> Dependencies f players player
singleConnectionDeps conn =
  Dependencies
    { requestBid = getBidSingle conn
    , requestViceTrump = getViceTrumpSingle conn
    , requestChiefTrump = getChiefTrumpSingle conn
    , requestPartner = getPartnerSingle conn
    , requestCard = getCardSingle conn
    }

preIndexedDeps ::
     ( MonadIO f
     , MonadError ServerError f
     , ToJSON player
     , Foldable players
     , MonadState playerCards f
     )
  => (player -> Lens' playerCards (Map Int PlayableCard))
  -> Connection
  -> Dependencies f players player
preIndexedDeps lens conn =
  (singleConnectionDeps conn)
    {requestBid = requestBid, requestCard = getCardPreIndexed lens conn}
  where
    requestBid player maxRaise _ = getBidPreIndexed lens conn player maxRaise

preIndexedUpdates ::
     ( MonadIO f
     , MonadError ServerError f
     , ToJSON player
     , ToJSON scores
     , MonadState playerCards f
     )
  => (player -> ASetter' playerCards (Map Int PlayableCard))
  -> Connection
  -> Updates f player scores
preIndexedUpdates lens conn =
  (singleConnectionUpdates conn) {dealUpdate = dealUpdatePreIndexed lens conn}

singleConnectionUpdates ::
     (MonadIO f, MonadError ServerError f, ToJSON player, ToJSON scores)
  => Connection
  -> Updates f player scores
singleConnectionUpdates conn =
  Updates
    { dealUpdate = dealUpdateSingle conn
    , biddingResultUpdate = biddingResultUpdateWS conn
    , trickWinnerUpdate = trickWinnerUpdateWS conn
    , scoresUpdate = scoresUpdateWS conn
    }

playMuSingleConnectionThreePlayers ::
     (MonadRandom m, MonadIO m, MonadError ServerError m, Bind m)
  => Connection
  -> EndCondition
  -> m (Tuple3 Score)
playMuSingleConnectionThreePlayers conn endcondition =
  evalStateT
    (playMuThreePlayersWithUpdates
       (preIndexedUpdates threeLens conn)
       (preIndexedDeps threeLens conn)
       endcondition)
    mempty

playMuSingleConnectionFourPlayers ::
     (MonadRandom m, MonadIO m, MonadError ServerError m, Bind m)
  => Connection
  -> EndCondition
  -> m (Tuple4 Score)
playMuSingleConnectionFourPlayers conn endcondition =
  evalStateT
    (playMuFourPlayersWithUpdates
       (preIndexedUpdates fourLens conn)
       (preIndexedDeps fourLens conn)
       endcondition)
    mempty

playMuSingleConnectionFivePlayers ::
     (MonadRandom m, MonadIO m, MonadError ServerError m, Bind m)
  => Connection
  -> EndCondition
  -> m (Tuple5 Score)
playMuSingleConnectionFivePlayers conn endcondition =
  evalStateT
    (playMuFivePlayersWithUpdates
       (preIndexedUpdates fiveLens conn)
       (preIndexedDeps fiveLens conn)
       endcondition)
    mempty

playMuSingleConnectionSixPlayers ::
     (MonadRandom m, MonadIO m, MonadError ServerError m, Bind m)
  => Connection
  -> EndCondition
  -> m (Tuple6 Score)
playMuSingleConnectionSixPlayers conn endcondition =
  evalStateT
    (playMuSixPlayersWithUpdates
       (preIndexedUpdates sixLens conn)
       (preIndexedDeps sixLens conn)
       endcondition)
    mempty

instance ToJSON Trump where
  toJSON (SuitTrump suit) = toJSON suit
  toJSON (RankTrump rank) = toJSON rank
  toJSON NoTrump          = String "no trump"
