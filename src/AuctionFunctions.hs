module AuctionFunctions
  ( Bid(..)
  , Player
  , AuctionResult(..)
  , Stalemate(..)
  , AuctionStatus(..)
  , AuctionState(..)
  , Trump(..)
  , bidTotals
  , auctionState
  , availableTrumps
  , cardTrumps
  , minus
  , auctionStatus
  , initialState
  , chief
  , remove
  , Winners(..)
  ) where

import           Cards
import           Control.Monad.State.Lazy
import           Data.List
import qualified Data.Map.Lazy            as Map
import           Data.Maybe

type Player = Int

data Trump
  = SuitTrump Suit
  | RankTrump Int
  | NoTrump
  deriving (Eq, Show)

data Bid
  = Pass
  | Raise [Card]

data AuctionStatus
  = Unfinished
  | Finished AuctionResult
  deriving (Eq, Show)

data Winners
  = ChiefOnly Player
  | ChiefAndVice Player
                 Player
  deriving (Eq, Show)

data AuctionResult
  = Result Winners
  | NoResult Stalemate
  deriving (Eq, Show)

data Stalemate
  = EklatNoPoints
  | Eklat { atFault  :: Player
          , affected :: [Player] }
  deriving (Eq, Show)

data AuctionState = AuctionState
  { cardsInHand :: Map.Map Player [Card]
  , passes      :: Int
  , cardsBid    :: Map.Map Player [Card]
  , lastToRaise :: [Player]
  }

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
  | x == y = ys
  | otherwise = y : (remove x ys)

minus :: Eq a => [a] -> [a] -> [a]
minus xs ys = foldl (flip remove) xs ys

auctionState :: Player -> Bid -> AuctionState -> AuctionState 
auctionState _ Pass state =
  state {passes = passes state + 1}
auctionState player (Raise cards) state = AuctionState
  { cardsBid = Map.insertWith (++) player cards $ cardsBid state
  , passes = 0
  , lastToRaise = player : lastToRaise state
  , cardsInHand = Map.insertWith (flip minus) player cards $ cardsInHand state
  }


initialState :: Map.Map Player [Card] -> AuctionState
initialState initialHands =
  AuctionState
    { cardsInHand = initialHands
    , cardsBid = Map.empty
    , lastToRaise = []
    , passes = 0
    }


type NumberOfPlayers = Int

bidTotals :: Map.Map Player [Card] -> Map.Map Player Int
bidTotals cardsBid = Map.map length cardsBid

auctionStatus :: NumberOfPlayers -> AuctionState -> AuctionStatus
auctionStatus numberOfPlayers AuctionState { cardsBid = cardsBid
                                           , passes = passes
                                           , lastToRaise = lastToRaise
                                           } =
  if passes < numberOfPlayers
    then Unfinished
    else Finished result
  where
    result =
      case leadersInOrderOfLastRaised of
        [] -> NoResult EklatNoPoints
        [chief] ->
          case vices of
            [vice] -> Result (ChiefAndVice chief vice)
            _      -> Result (ChiefOnly chief)
        lastLeaderToRaise:others ->
          NoResult Eklat {atFault = lastLeaderToRaise, affected = others}
    totals = bidTotals cardsBid
    maxBid = maximum $ 0 : Map.elems totals
    leaders = Map.keys $ Map.filter (== maxBid) $ totals
    leadersInOrderOfLastRaised = sortOn lastToRaiseIndex leaders
    lastToRaiseIndex a = elemIndex a lastToRaise
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

validBid :: [Card] -> AuctionState -> Bool
validBid bid state = length bid <= oneAboveMax
  where
    oneAboveMax = (maximum . Map.elems . bidTotals $ cardsBid state) + 1

chief :: Winners -> Player
chief (ChiefOnly chief')      = chief'
chief (ChiefAndVice chief' _) = chief'

cardTrumps :: [Card] -> [Trump]
cardTrumps cards = nub . concatMap suitAndRank $ cards where
  suitAndRank card = [SuitTrump $ suit card, RankTrump $ rank card]


availableTrumps :: [Card] -> [Trump]
availableTrumps cards = NoTrump : cardTrumps cards
