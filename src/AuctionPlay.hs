module AuctionPlay
  ( Interactions(..)
  , auctionRound
  , bidding
  , Trumps(..)
  , Trump(..)
  , TrumpsAndTeams(..)
  , Teams(..)
  , FinishedAuction(..)
  ) where

import           AuctionFunctions
import           Cards
import           Control.Monad.State.Lazy
import           Data.List.Index
import qualified Data.Map.Lazy            as Map
import           Data.Maybe
import           Util

data TrumpsAndTeams player =
  TrumpsAndTeams Trumps
                 (Teams player)
                 TopBid
                 (CardPositions player)
  deriving (Show)

data Teams player
  = ChiefAlone player
  | ChiefAndPartner player
                    player
  deriving (Show)

data FinishedAuction player
  = Successful (TrumpsAndTeams player)
  | Unsuccessful (Stalemate player)
  deriving (Show)

data FinishedAuction2 player
  = ThreePlayerAuction Trumps
  | FullGameAuction { chosenPartner :: player
                    , chosenTrumps  :: Trumps }

data Interactions f player = Interactions
  { getBid     :: Int -> player -> [Card] -> f Bid
  , getTrump   :: player -> [Trump] -> f Trump
  , getPartner :: player -> [player] -> f player
  }

type NumberOfPlayers = Int

bidding2 ::
     (Ord player, Monad f)
  => (Int -> player -> [Card] -> f Bid)
  -> [(player, [Card])]
  -> f (FinishedBidding player)
bidding2 getBid playerHands =
  evalStateT (bidding2' numberOfPlayers getBid playerSequence) $
  initialState playerHands
  where
    players = map fst playerHands
    playerSequence = cycle players
    numberOfPlayers = length players

bidding ::
     (Ord player, Monad f)
  => (Int -> player -> [Card] -> f Bid)
  -> [player]
  -> StateT (AuctionState player) f (AuctionResult player)
bidding getBid players = bidding' numberOfPlayers getBid playerSequence
  where
    playerSequence = cycle players
    numberOfPlayers = length players

bidding2' ::
     (Ord player, Monad f)
  => NumberOfPlayers
  -> (Int -> player -> [Card] -> f Bid)
  -> [player]
  -> StateT (AuctionState player) f (FinishedBidding player)
bidding2' numberOfPlayers getBid (thisPlayer:nextPlayers) = do
  state <- get
  let maxBid =
        maximum
          (0 : (map length . Map.elems . cardsOnTable $ auctionPositions state))
      cards = findOrEmptyList thisPlayer . cardsInHand $ auctionPositions state
      numberOfPasses = passes state
      currentTotal =
        length . findOrEmptyList thisPlayer . cardsOnTable $
        auctionPositions state
      maxBidAllowed = maxBid + 1 - currentTotal
  if numberOfPasses == numberOfPlayers
    then return $
         finishBidding
           numberOfPlayers
           (auctionPositions state)
           (lastToRaise state)
    else do
      bid <- lift $ getBid maxBidAllowed thisPlayer cards
      modify $ auctionState thisPlayer bid
      bidding2' numberOfPlayers getBid nextPlayers

bidding' ::
     (Ord player, Monad f)
  => NumberOfPlayers
  -> (Int -> player -> [Card] -> f Bid)
  -> [player]
  -> StateT (AuctionState player) f (AuctionResult player)
bidding' numberOfPlayers getBid (thisPlayer:nextPlayers) = do
  state <- get
  case auctionStatus numberOfPlayers state of
    Finished result -> return result
    Unfinished -> do
      bid <- lift $ getBid maxBidAllowed thisPlayer cards
      modify $ auctionState thisPlayer bid
      bidding' numberOfPlayers getBid nextPlayers
      where cards =
              findOrEmptyList thisPlayer . cardsInHand $ auctionPositions state
            maxBid =
              maximum
                (0 :
                 (map length . Map.elems . cardsOnTable $ auctionPositions state))
            currentTotal =
              length . findOrEmptyList thisPlayer . cardsOnTable $
              auctionPositions state
            maxBidAllowed = maxBid + 1 - currentTotal

getTrumps ::
     (Ord player, Monad f)
  => (player -> [Trump] -> f Trump)
  -> Winners player
  -> Map.Map player [Card]
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
     (Eq player, Ord player, Monad f)
  => Interactions f player
  -> [(player, [Card])]
  -> f (FinishedAuction player)
auctionRound interactions startingHands = do
  (result, state) <- runStateT bidding' $ initialState startingHands
  case result of
    Result winners cardPositions -> do
      trumps <-
        getTrumps (getTrump interactions) winners . cardsOnTable $
        auctionPositions state
      teams <-
        if numberOfPlayers == 3
          then return $ ChiefAlone chief'
          else fmap (ChiefAndPartner chief') . getPartner interactions chief' $
               potentialPartners winners
      let topBid = undefined
      return . Successful $ TrumpsAndTeams trumps teams topBid cardPositions
      where chief' = chief winners
    NoResult stalemate -> return $ Unsuccessful stalemate
  where
    players = fst <$> startingHands
    numberOfPlayers = length players
    bidding' = bidding (getBid interactions) players
    potentialPartners (ChiefOnly chief) = remove chief players
    potentialPartners (ChiefAndVice chief vice) =
      remove chief $ remove vice players
