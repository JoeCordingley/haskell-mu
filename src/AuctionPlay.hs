module AuctionPlay
  ( Interactions(..)
  ) where

import           AuctionFunctions
import           Cards
import           Control.Monad.State.Lazy
import           Data.List.Index
import qualified Data.Map.Lazy            as Map
import           Data.Maybe

data Trumps
  = SingleTrump Trump
  | HigherLower Trump
                Trump

data TrumpsAndTeams =
  TrumpsAndTeams Trumps
                 Teams

data Teams
  = ChiefAlone Player
  | ChiefAndPartner Player
                    Player

data FinishedAuction
  = Successful TrumpsAndTeams
  | Unsuccessful Stalemate

data Interactions f = Interactions
  { getBid     :: Player -> f Bid
  , getTrump   :: Player -> f Trump
  , getPartner :: Player -> f Player
  , getStatus  :: Bid -> f AuctionStatus
  }

bidding ::
     Monad f
  => (Player -> f Bid)
  -> (Bid -> f AuctionStatus)
  -> [Player]
  -> f AuctionResult
bidding getBid placeBid (thisPlayer:nextPlayers) = do
  bid <- getBid thisPlayer
  status <- placeBid bid
  case status of
    Finished result -> return result
    Unfinished      -> bidding getBid placeBid nextPlayers

getTrumps :: Monad f => (Player -> f Trump) -> Winners -> f Trumps
getTrumps getTrump (ChiefOnly chief) = fmap SingleTrump $ getTrump chief
getTrumps getTrump (ChiefAndVice chief vice) = do
  lower <- getTrump vice
  higher <- getTrump chief
  return $ HigherLower higher lower

auctionRound :: Monad f => Interactions f -> [Player] -> f FinishedAuction
auctionRound interactions players = do
  result <-
    bidding (getBid interactions) (getStatus interactions) playerSequence
  case result of
    Result winners -> do
      trumps <- getTrumps (getTrump interactions) winners
      teams <-
        getTeams (getPartner interactions) (chief winners) numberOfPlayers
      return . Successful $ TrumpsAndTeams trumps teams
    NoResult stalemate -> return $ Unsuccessful stalemate
  where
    numberOfPlayers = length players
    playerSequence = cycle players

type NumberOfPlayers = Int

getTeams ::
     Monad f => (Player -> f Player) -> Player -> NumberOfPlayers -> f Teams
getTeams getPartner chief 3 = return $ ChiefAlone chief
getTeams getPartner chief _ = fmap (ChiefAndPartner chief) $ getPartner chief
--auctionRound :: Monad f => Interactions f -> [Player] -> f FinishedAuction
--auctionRound Interactions { getBid = getBid
--                          , getTrump = getTrump
--                          , getPartner = getPartner
--                          } players = do
--  result <- bidding getBid playerSequence numberOfPlayers
--  case result of
--    Result winners -> do
--      trumps <- getTrumps getTrump winners
--      teams <- getTeams getPartner chief numberOfPlayers
--      return . Successful $ TrumpsAndTeams trumps teams
--      where chief = chiefOf winners
--    NoResult stalemate -> return $ Unsuccessful stalemate
--  where
--    playerSequence = cycle players
--    numberOfPlayers = length players
--bidding ::
--     Monad f
--  => (Player -> f Bid)
--  -> [Player]
--  -> NumberOfPlayers
--  -> f AuctionResult
--bidding getBid playerSequence numberOfPlayers = play playerSequence initialState
--  where
--    play = evalStateT . stateful
--    stateful (thisPlayer:nextPlayers) = do
--      status <- StateT $ placeBid numberOfPlayers getBid thisPlayer
--      case status of
--        Finished result -> return result
--        Unfinished      -> stateful nextPlayers
