module Auction
  ( Bid(..)
  --, playAuction
  --, Actions(..)
  , Player
  , AuctionResult(..)
  , AuctionStatus(..)
  , playerBidsToStatus
  ) where

import           Cards
import           Data.List
import qualified Data.Map.Lazy as Map
import           Data.Maybe

type Player = Int

data Bid
  = Pass
  | Raise [Card]

data AuctionStatus
  = Unfinished
  | Finished AuctionResult deriving (Eq, Show)

data AuctionResult
  = EklatNoPoints
  | Eklat { atFault  :: Player
          , affected :: [Player] }
  | ChiefAndVice Player
                 Player
  | ChiefOnly Player
  deriving (Eq, Show)

data AuctionState = AuctionState
  { passes      :: Int
  , totals      :: Map.Map Player [Card]
  , lastToRaise :: [Player]
  }

data Interactions f = Interactions
  { getBid :: Player -> f Bid
  , endingInteractions :: EndingInteractions f
  }

data Trump = Trump

data EndingInteractions f = EndingInteractions
  { getTrump :: Player -> f Trump
  , getPartner :: Player -> f Player
  }

data FinishedAuction = FinishedAuction

auctionState :: AuctionState -> (Player, Bid) -> AuctionState
auctionState state@AuctionState {passes = num} (_, Pass) =
  state {passes = num + 1}
auctionState AuctionState {totals = totals, lastToRaise = lastToRaise} (player, Raise cards) =
  AuctionState
    { totals = Map.insertWith (++) player cards totals
    , passes = 0
    , lastToRaise = player : lastToRaise
    }

initialState = AuctionState {totals = Map.empty, lastToRaise = [], passes = 0}

playerBidsToStatus :: [Player] -> [ Bid] -> AuctionStatus
playerBidsToStatus players bids =
  auctionStatus numberOfPlayers $ foldl auctionState initialState playerBids where
  numberOfPlayers = length players
  playerBids = cycle players `zip` bids

auctionStatus :: Int -> AuctionState -> AuctionStatus
auctionStatus numberOfPlayers AuctionState { totals = totals
                                                  , passes = passes
                                                  , lastToRaise = lastToRaise
                                                  } =
  if passes < numberOfPlayers
    then Unfinished
    else Finished result
  where
    result =
      case leadersInOrderOfLastRaised of
        [] -> EklatNoPoints
        [chief] ->
          case vices of
            [vice] -> ChiefAndVice chief vice
            _      -> ChiefOnly chief
        lastLeaderToRaise:others ->
          Eklat {atFault = lastLeaderToRaise, affected = others}
    bidTotals = Map.map length totals
    maxBid = maximum $ 0 : Map.elems bidTotals
    leaders = Map.keys $ Map.filter (== maxBid) bidTotals
    leadersInOrderOfLastRaised = sortOn lastToRaiseIndex leaders
    lastToRaiseIndex a = elemIndex a lastToRaise
    vices =
      case leaders of
        [leader] -> map fst . maximumsBy compareByCards $ Map.toList otherTotals
          where otherTotals = Map.delete leader totals
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

playAuction :: Monad f => Interactions f -> [Player] -> f AuctionResult
playAuction Interactions{ getBid = getBid } players =
  play playerSequence initialState where
    playerSequence = cycle players
    playerNumber = length players
    play (thisPlayer:nextPlayers) state = do
      bid <- getBid thisPlayer
      case status bid of
        Finished result -> return result
        Unfinished -> play nextPlayers $ newState bid
      where 
        status = auctionStatus playerNumber . newState
        newState bid = auctionState state (thisPlayer, bid)

finishAuction :: Monad f => EndingInteractions f -> AuctionResult -> f FinishedAuction
finishAuction EndingInteractions{getTrump = getTrump} = undefined
