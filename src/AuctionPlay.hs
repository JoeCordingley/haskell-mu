module AuctionPlay
  ( Interactions(..)
  , auctionRound
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
                Trump deriving Show

data TrumpsAndTeams =
  TrumpsAndTeams Trumps
                 Teams deriving Show

data Teams
  = ChiefAlone Player
  | ChiefAndPartner Player
                    Player deriving Show

data FinishedAuction
  = Successful TrumpsAndTeams
  | Unsuccessful Stalemate deriving Show

data Interactions f = Interactions
  { getBid     :: Int -> Player -> [Card] -> f Bid
  , getTrump   :: Player -> [Trump] -> f Trump
  , getPartner :: Player -> [Player] -> f Player
  }

type NumberOfPlayers = Int

findOrEmptyList :: (Ord k) => k -> Map.Map k [a] -> [a]
findOrEmptyList = Map.findWithDefault []

bidding ::
     Monad f
  => NumberOfPlayers
  -> (Int -> Player -> [Card] -> f Bid)
  -> [Player]
  -> StateT AuctionState f AuctionResult
bidding numberOfPlayers getBid (thisPlayer:nextPlayers) = do
  state <- get
  case auctionStatus numberOfPlayers state of
    Finished result -> return result
    Unfinished -> do
      bid <- lift $ getBid maxBidAllowed thisPlayer cards
      modify $ auctionState thisPlayer bid
      bidding numberOfPlayers getBid nextPlayers
      where 
        cards = findOrEmptyList thisPlayer $ cardsInHand state
        maxBid = maximum  (0 : (map length . Map.elems $ cardsBid state))
        currentTotal = length . findOrEmptyList thisPlayer $ cardsBid state
        maxBidAllowed = maxBid + 1 - currentTotal

getTrumps ::
     Monad f
  => (Player -> [Trump] -> f Trump)
  -> Winners
  -> Map.Map Player [Card]
  -> f Trumps
getTrumps getTrump (ChiefOnly chief) cardsBid =
  fmap SingleTrump . getTrump chief . cardTrumps $
  findOrEmptyList chief cardsBid
getTrumps getTrump (ChiefAndVice chief vice) cardsBid = do
  viceTrump <- getTrump vice . cardTrumps $ findOrEmptyList vice cardsBid
  chiefTrump <-
    getTrump chief . (NoTrump :) . remove viceTrump . cardTrumps $
    findOrEmptyList chief cardsBid
  return $ HigherLower chiefTrump viceTrump

auctionRound ::
     Monad f => Interactions f -> Map.Map Player [Card] -> f FinishedAuction
auctionRound interactions startingHands = do
  (result, state) <- runStateT bidding' $ initialState startingHands
  case result of
    Result winners -> do
      trumps <- getTrumps (getTrump interactions) winners $ cardsBid state
      teams <-
        if numberOfPlayers == 3
          then return $ ChiefAlone chief'
          else fmap (ChiefAndPartner chief') . getPartner interactions chief' $
               potentialPartners winners
      return . Successful $ TrumpsAndTeams trumps teams
      where chief' = chief winners
    NoResult stalemate -> return $ Unsuccessful stalemate
  where
    players = Map.keys startingHands
    numberOfPlayers = length players
    playerSequence = cycle players
    bidding' = bidding numberOfPlayers (getBid interactions) playerSequence
    potentialPartners (ChiefOnly chief) = remove chief players
    potentialPartners (ChiefAndVice chief vice) =
      remove chief $ remove vice players
