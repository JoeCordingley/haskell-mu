module New.Bidding where

import New.GamePlay
import New.Util
import Cards
import AuctionFunctions(Bid(..), Winners(..), viceOrdering)
import Control.Monad.State.Class
import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.List hiding (sortOn)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as Semi
import Data.Monoid
import Data.Semigroup.Foldable
import Data.Tuple.Homogenous
import Control.Monad.State.Lazy
import New.TupleInstances


data FinishedBidding  player
  = Successful (SuccessfulBidding player)
  | Unsuccessful (Stalemate  player)

data SuccessfulBidding player = SuccessfulBidding 
  { biddingWinners :: Winners player
  }

data Stalemate player
  = EklatNoPoints
  | Eklat { topBid :: Int
          , atFault  :: player
          , affected :: NonEmpty player 
          }
  deriving (Eq, Show)

initialPositions :: [Card] -> CardPositions 
initialPositions cards = CardPositions{inHand = cards, onTable = []}

type GetBid f player = player -> [Card] -> f Bid

runAuction
  :: (Monad f, Monoid bids, Cycling player) =>
     (player -> f Bid)
     -> (player -> [Card] -> bids) -> Int -> player -> f (bids, [player])
runAuction getBid raise numberOfPlayers firstPlayer = evalStateT (runAuction' numberOfPlayers) firstPlayer where
  runAuction' 0 = return $ mempty
  runAuction' passesLeft = do
    player <- players
    bid <- lift $ getBid player
    case bid of 
      Pass -> runAuction' (passesLeft - 1)
      Raise cards -> ((raise player cards , [player]) <>) <$> runAuction' numberOfPlayers

tallyAuction
  :: (Eq player, Foldable1 players) => 
     players (player, [Card])
     -> FinishedBidding player
tallyAuction playerCards = 
  if topBid == 0 then Unsuccessful(EklatNoPoints)
  else case NE.reverse maxBidders' of
    chief :| [] -> Successful(successful chief)
    lastPlayerToRaise :| penultimate : others -> Unsuccessful(Eklat topBid lastPlayerToRaise (penultimate :| others))
  where 
    MaxBidders (Semi.Max topBid) maxBidders' = foldMap1 playerBids playerCards
    successful = SuccessfulBidding . winners
    winners chief = case vices chief of
      [vice] -> ChiefAndVice chief vice
      _ -> ChiefOnly chief
    vices chief = maxBidders . foldMap viceBids . NE.filter (notPlayer chief) $ toNonEmpty playerCards
    notPlayer player = (/=player) . fst
    viceBids (player, cards) = MaxBidders (ViceBid cards) [player] 
    playerBids (player, cards) = MaxBidders (Semi.Max (length cards)) (player :| [])

playAuction
  :: (Foldable1 players, Traversable players, Eq player, Monad f,
      Monoid (players [Card]), Cycling player, Applicative players) =>
     (player -> f Bid)
     -> (player -> [Card] -> players [Card])
     -> players player
     -> Int
     -> player
     -> f (FinishedBidding player)
playAuction getBid raise players numberOfPlayers firstPlayer = 
  tallyAuction . uncurry bidsByLastRaise . over _1 (playerCards players) <$> runAuction getBid raise numberOfPlayers firstPlayer

playerCards players cards = (,) <$> players <*> cards

bidsByLastRaise :: (Eq player, Traversable f) => f (player, cards) -> [player] -> f (player, cards)
bidsByLastRaise bids lastRaised = sortOn playerIndex bids where
  playerIndex (player, _) = elemIndex player lastRaised

play3 getBid firstPlayer = playAuction getBid raiseThree tuple3Players 3 firstPlayer
  
data ThreePlayers = OneOfThree | TwoOfThree | ThreeOfThree deriving (Eq, Enum, Show)

raiseThree OneOfThree cards = Tuple3 (cards,  [], [])
raiseThree TwoOfThree cards = Tuple3 ([],  cards, [])
raiseThree ThreeOfThree cards = Tuple3 ([], [], cards)

tuple3Players = Tuple3 (OneOfThree, TwoOfThree, ThreeOfThree)

threePlayerSequence = cycle $ enumFrom OneOfThree

players :: (Cycling player, MonadState player m) => m player
players = state nextPlayer where
  nextPlayer player = (player, successor player)

class Cycling a where
  successor :: a -> a

instance Cycling ThreePlayers where
  successor ThreeOfThree = OneOfThree 
  successor other = succ other


newtype ViceBid = ViceBid [Card]

instance Eq ViceBid where
  ViceBid a == ViceBid b = viceOrdering a b == EQ

instance Ord ViceBid where
  ViceBid a <= ViceBid b = viceOrdering a b /= GT

instance Zeroed ViceBid where 
  zero = ViceBid []

data MaxBidders players bid = MaxBidders
  { maxBid :: bid
  , maxBidders :: players
  }

instance (Ord bid, Semigroup players) => Semigroup (MaxBidders players bid) where
  MaxBidders bidA playersA <> MaxBidders bidB playersB = case compare bidA bidB of
    GT -> MaxBidders bidA playersA
    LT -> MaxBidders bidB playersB
    EQ -> MaxBidders bidA (playersA <> playersB)

class Ord a => Zeroed a where
  zero :: a

instance (Monoid players, Zeroed bid) => Monoid (MaxBidders players bid) where
  mempty = MaxBidders zero mempty


