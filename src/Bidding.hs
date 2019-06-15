module Bidding
  ( InitialHands
  , MaxBid
  , Bid(..)
  , FinishedBidding(..)
  , runBidding
  ) where

import           Cards
import           Control.Monad.State.Lazy
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map
import           Util
import Control.Monad.Reader

type InitialHands player = [(player, [Card])]

type MaxBid = Int

type GetBid f player = (Int -> player -> [Card] -> f Bid)

data Bid
  = Pass
  | Raise [Card] deriving Show

data FinishedBidding player = FinishedBidding
  { finishedCardsInHand  :: Map player [Card]
  , finishedPlayerRaises :: [(player, [Card])]
  }

data BiddingState player = BiddingState
  { cardsInHandSoFar  :: Map player [Card]
  , passesSoFar       :: Int
  , playerRaisesSoFar :: [(player, [Card])]
  }

initialState :: Ord player => [(player, [Card])] -> BiddingState player
initialState initialHands =
  BiddingState
    { cardsInHandSoFar = Map.fromList initialHands
    , passesSoFar = 0
    , playerRaisesSoFar = []
    }


runBidding ::
     (Ord player, Monad f)
  => [(player, [Card])]
  -> ReaderT (GetBid f player) f (FinishedBidding player)
runBidding initialHands =
  evalStateT (runBiddingStateful numberOfPlayers playerSequence) $
  initialState initialHands
  where
    playerSequence = cycle players
    players = map fst initialHands
    numberOfPlayers = length players

type NumberOfPlayers = Int

runBiddingStateful ::
     (Ord player, Monad f)
  => NumberOfPlayers
  -> [player]
  -> StateT (BiddingState player) (ReaderT (GetBid f player) f) (FinishedBidding player)
runBiddingStateful numberOfPlayers = runBiddingStateful'
  where
    runBiddingStateful' (thisPlayer:laterPlayers) = do
      passes <- gets passesSoFar
      if (passes == numberOfPlayers)
        then gets finishBidding
        else modifyF (getSingleBid thisPlayer) *>
             runBiddingStateful' laterPlayers

getSingleBid ::
     (Functor f, Ord player)
  => player
  -> BiddingState player
  -> ReaderT (GetBid f player) f (BiddingState player)
getSingleBid player state = ReaderT getSingleBid' where
  getSingleBid' getBid = newBiddingState state player <$> getBid maxBid player cards
  bidTotals =
    foldr (uncurry $ Map.insertWith (+)) Map.empty . map (second length) $
    playerRaisesSoFar state
  second f (a, b) = (a, f b)
  topBid = maximum $ 0 : Map.elems bidTotals
  playerTotal = Map.findWithDefault 0 player bidTotals
  maxBid = topBid + 1 - playerTotal
  cards = findOrEmptyList player $ cardsInHandSoFar state

newBiddingState ::
     Ord player => BiddingState player -> player -> Bid -> BiddingState player
newBiddingState state _ Pass = state {passesSoFar = (passesSoFar state) + 1}
newBiddingState state player (Raise cards) =
  BiddingState
    { cardsInHandSoFar =
        Map.insertWith (flip minus) player cards $ cardsInHandSoFar state
    , passesSoFar = 0
    , playerRaisesSoFar = (player, cards) : playerRaisesSoFar state
    }

modifyF :: Monad f => (s -> f s) -> StateT s f ()
modifyF f = get >>= lift . f >>= put

finishBidding :: (BiddingState player) -> (FinishedBidding player)
finishBidding state =
  FinishedBidding
    { finishedCardsInHand = cardsInHandSoFar state
    , finishedPlayerRaises = reverse $ playerRaisesSoFar state
    }
