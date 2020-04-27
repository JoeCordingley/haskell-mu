module AuctionFunctions
  ( Bid(..)
  , AuctionResult(..)
  , Stalemate(..)
  , AuctionStatus(..)
  , AuctionState(..)
  , Trump(..)
  , bidTotals
  , auctionState
  , cardTrumps
  , minus
  , auctionStatus
  , initialState
  , chief
  , Winners(..)
  , CardPositions(..)
  , FinishedBidding(..)
  , TopBid
  , finishBidding
  , SuccessfulBidding(..)
  , viceOrdering
  ) where

import           Cards
import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.List
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map
import           Data.Maybe
import           Util

data Bid
  = Pass
  | Raise [Card]
  deriving (Show, Eq)

type TopBid = Int


data FinishedBidding player
  = Successful (SuccessfulBidding player)
  | Unsuccessful (Stalemate player)

data SuccessfulBidding player = SuccessfulBidding 
  { biddingWinners :: Winners player
  , successfulTopBid :: Int
  , biddingPositions :: CardPositions player
  }

data CardPositions player = CardPositions
  { cardsInHand  :: Map player [Card]
  , cardsOnTable :: Map player [Card]
  } deriving (Eq, Show)

data AuctionStatus player
  = Unfinished
  | Finished (AuctionResult player)
  deriving (Eq, Show)

data Winners player
  = ChiefOnly player
  | ChiefAndVice player
                 player
  deriving (Eq, Show)

data AuctionResult player
  = Result (Winners player)
           (CardPositions player)
  | NoResult (Stalemate player)
  deriving (Eq, Show)

data Stalemate player
  = EklatNoPoints
  | Eklat { atFault  :: player
          , affected :: [player]
          , topBid   :: Int }
  deriving (Eq, Show)

data AuctionState player = AuctionState
  { auctionPositions :: CardPositions player
  , passes           :: Int
  , lastToRaise      :: [player]
  }

auctionState ::
     Ord player => player -> Bid -> AuctionState player -> AuctionState player
auctionState _ Pass state = state {passes = passes state + 1}
auctionState player (Raise cards) state =
  AuctionState
    { auctionPositions =
        CardPositions
          { cardsOnTable =
              Map.insertWith (++) player cards . cardsOnTable $
              auctionPositions state
          , cardsInHand =
              Map.insertWith (flip minus) player cards . cardsInHand $
              auctionPositions state
          }
    , passes = 0
    , lastToRaise = player : lastToRaise state
    }

initialState :: Ord player => [(player, [Card])] -> AuctionState player
initialState initialHands =
  AuctionState
    { auctionPositions =
        CardPositions
          {cardsInHand = Map.fromList initialHands, cardsOnTable = Map.empty}
    , lastToRaise = []
    , passes = 0
    }

type NumberOfPlayers = Int

bidTotals :: Map player [Card] -> Map player Int
bidTotals cardsBid = Map.map length cardsBid

finishBidding ::
     Ord player
  => NumberOfPlayers
  -> CardPositions player
  -> [player]
  -> FinishedBidding player
finishBidding numberOfPlayers cardPositions lastToRaise =
  if null lastToRaise
    then Unsuccessful EklatNoPoints
    else case leadersInOrderOfLastRaised of
           [chief] ->
             case vices of
               [vice] -> finishWith $ ChiefAndVice chief vice
               _      -> finishWith $ ChiefOnly chief
           lastLeaderToRaise:others ->
             Unsuccessful
               Eklat
                 { atFault = lastLeaderToRaise
                 , affected = others
                 , topBid = maxBid
                 }
  where
    finishWith winners =
      Successful SuccessfulBidding
        { biddingWinners = winners
        , biddingPositions = cardPositions
        , successfulTopBid = maxBid
        }
    totals = bidTotals cardsBid
    maxBid = maximum $ 0 : Map.elems totals
    leaders = Map.keys $ Map.filter (== maxBid) $ totals
    leadersInOrderOfLastRaised = sortOn lastToRaiseIndex leaders
    lastToRaiseIndex a = elemIndex a lastToRaise
    cardsBid = cardsOnTable cardPositions
    vices =
      case leaders of
        [leader] -> map fst . maximumsBy compareByCards $ Map.toList otherTotals
          where otherTotals = Map.delete leader cardsBid
                compareByCards (_, as) (_, bs) = viceOrdering as bs
        _ -> []

auctionStatus ::
     Ord player
  => NumberOfPlayers
  -> AuctionState player
  -> AuctionStatus player
auctionStatus numberOfPlayers state =
  if passes' < numberOfPlayers
    then Unfinished
    else Finished result
  where
    result =
      if null lastToRaise'
        then NoResult EklatNoPoints
        else case leadersInOrderOfLastRaised of
               [chief] ->
                 case vices of
                   [vice] -> Result (ChiefAndVice chief vice) (cardPositions)
                   _      -> Result (ChiefOnly chief) (cardPositions)
               lastLeaderToRaise:others ->
                 NoResult
                   Eklat
                     { atFault = lastLeaderToRaise
                     , affected = others
                     , topBid = maxBid
                     }
    lastToRaise' = lastToRaise state
    passes' = passes state
    totals = bidTotals cardsBid
    maxBid = maximum $ 0 : Map.elems totals
    leaders = Map.keys $ Map.filter (== maxBid) $ totals
    leadersInOrderOfLastRaised = sortOn lastToRaiseIndex leaders
    lastToRaiseIndex a = elemIndex a lastToRaise'
    cardPositions = auctionPositions state
    cardsBid = cardsOnTable cardPositions
    vices =
      case leaders of
        [leader] -> map fst . maximumsBy compareByCards $ Map.toList otherTotals
          where otherTotals = Map.delete leader cardsBid
                compareByCards (_, as) (_, bs) = viceOrdering as bs
        _ -> []

compareLength :: [a] -> [b] -> Ordering
compareLength as bs = compare (length as) (length bs)

compareNs :: Rank -> [Card] -> [Card] -> Ordering
compareNs n as bs = compare (numberOfNs as) (numberOfNs bs)
  where
    numberOfNs = length . filter (== n) . map rank

viceComparisons :: [[Card] -> [Card] -> Ordering]
viceComparisons = compareLength : map compareNs [9,8 .. 1]

maximumsBy :: (a -> a -> Ordering) -> [a] -> [a]
maximumsBy f = foldr maximums []
  where
    maximums a [] = [a]
    maximums a as@(a':_) =
      case f a a' of
        GT -> [a]
        EQ -> a : as
        LT -> as

compareBy :: Traversable t => t (a -> b -> Ordering) -> a -> b -> Ordering
compareBy fs a b =
  case find (/= EQ) $ fmap (\c -> c a b) fs of
    Just unequal -> unequal
    Nothing      -> EQ

viceOrdering :: [Card] -> [Card] -> Ordering
viceOrdering = compareBy viceComparisons

validBid :: [Card] -> AuctionState player -> Bool
validBid bid state = length bid <= oneAboveMax
  where
    oneAboveMax =
      (maximum . Map.elems . bidTotals . cardsOnTable $ auctionPositions state) +
      1

chief :: Winners player -> player
chief (ChiefOnly chief')      = chief'
chief (ChiefAndVice chief' _) = chief'

cardTrumps :: [Card] -> [Trump]
cardTrumps cards = nub . concatMap suitAndRank $ cards
  where
    suitAndRank card = [SuitTrump $ suit card, RankTrump $ rank card]
