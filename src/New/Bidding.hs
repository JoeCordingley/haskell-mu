--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module New.Bidding where

import New.GamePlay
import New.Exercise
import Cards
import AuctionFunctions(Bid(..), Winners(..), viceOrdering)
import Control.Monad.State.Class
import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup as Semi
import Data.Monoid

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
  :: (Monad f, Monoid raises) =>
     (player -> f Bid)
     -> (player -> [Card] -> raises) -> f player -> Int -> f raises
runAuction getBid raise players numberOfPlayers = runAuction' numberOfPlayers where
  runAuction' 0 = return $ mempty
  runAuction' passesLeft = do
    player <- players
    bid <- getBid player
    case bid of 
      Pass -> runAuction' (passesLeft - 1)
      Raise cards -> (raise player cards <>) <$> runAuction' numberOfPlayers

tallyAuction
  :: Eq player => Getting (MaxBid (NonEmpty player) (Semi.Max Int)) playerCards (PlayerCards player)
     -> Getting (MaxBid [player] ViceBid) playerCards (PlayerCards player)
     -> playerCards
     -> FinishedBidding player
tallyAuction gettingTotals gettingViceBids playerCards = 
  if topBid == 0 then Unsuccessful(EklatNoPoints)
  else case NE.reverse maxBidders of
    chief :| [] -> Successful(successful chief)
    lastPlayerToRaise :| penultimate : others -> Unsuccessful(Eklat topBid lastPlayerToRaise (penultimate :| others))
  where 
    MaxBid (Semi.Max topBid) maxBidders = foldMapOf gettingTotals playerBids playerCards
    successful = SuccessfulBidding . winners
    winners chief = case vices chief of
      [vice] -> ChiefAndVice chief vice
      _ -> ChiefOnly chief
    vices chief = maxBidPlayers $ foldMapOf (gettingViceBids . filtered (notPlayer chief)) viceBids playerCards
    notPlayer player = (/=player) . playerCardsPlayer
    viceBids (PlayerCards player cards) = MaxBid (ViceBid cards) [player] 
    playerBids (PlayerCards player cards) = MaxBid (Semi.Max (length cards)) (player :| [])

playAuction getBid raise players numberOfPlayers gettingTotals gettingViceBids = tallyAuction gettingTotals gettingViceBids <$> runAuction getBid raise players numberOfPlayers
  
data ThreePlayers = OneOfThree | TwoOfThree | ThreeOfThree deriving (Eq, Enum, Show)

raiseThree OneOfThree cards = Three cards [] []
raiseThree TwoOfThree cards = Three [] cards []
raiseThree ThreeOfThree cards = Three [] [] cards

threePlayerSequence = cycle $ enumFrom OneOfThree

players :: (Cycling player, MonadState player m) => m player
players = state nextPlayer where
  nextPlayer player = (player, successor player)

class Cycling a where
  successor :: a -> a

instance Cycling ThreePlayers where
  successor ThreeOfThree = OneOfThree 
  successor other = succ other

instance Semigroup m => Semigroup (Three m) where
  Three l1 l2 l3 <> Three r1 r2 r3 = Three (l1 <> r1) (l2 <> r2) (l3 <> r3)

instance Monoid m => Monoid (Three m) where
  mempty = Three mempty mempty mempty

raiseAndRecordThree player cards = (raiseThree player cards, [player])


threePlayerAuction getBid = playAuction getBid raiseAndRecordThree players 3 

nubrBy :: (a -> a -> Bool) ->  [a] -> [a]
nubrBy f = reverse . nubBy f . reverse

data PlayerCards player = PlayerCards 
  { playerCardsPlayer:: player 
  , playerCardsCards :: [Card]
  }

t2 :: Three (PlayerCards Int)
t2 = undefined
t = tallyAuction threeFold threeFold t2

data ViceBid = ViceBid [Card]
instance Eq ViceBid where
  ViceBid a == ViceBid b = viceOrdering a b == EQ
instance Ord ViceBid where
  ViceBid a <= ViceBid b = viceOrdering a b /= GT

instance Zeroed ViceBid where 
  zero = ViceBid []

data MaxBid players bid = MaxBid
  { bid :: bid
  , maxBidPlayers :: players
  }

instance (Ord bid, Semigroup players) => Semigroup (MaxBid players bid) where
  MaxBid bidA playersA <> MaxBid bidB playersB = case compare bidA bidB of
    GT -> MaxBid bidA playersA
    LT -> MaxBid bidB playersB
    EQ -> MaxBid bidA (playersA <> playersB)

class Ord a => Zeroed a where
  zero :: a

instance (Monoid players, Zeroed bid) => Monoid (MaxBid players bid) where
  mempty = MaxBid zero mempty

winners f1 f2 f3 max playerCards = toListOf (f1 . filtered ((==max) . view f2) . f3)

maxBid2 fold cards traversal playerCards = maximum1Of (fold ) (over (traversal . cards) length playerCards)

--maxBid playerCards = maximum1Of threeFold (over threeFold length playerCards)

data Three a = Three a a a

threeFold :: (Apply f) => (a -> f b) -> Three a -> f (Three b) 
threeFold f (Three a b c) = Three <$> f a <.> f b <.> f c

playerCards :: Three [Int]
playerCards = Three [1,2] [1] [3]

playerBidSizes f = over (f) (length ) playerCards

maxKeys three = toListOf (threeFold . filtered hasMax . _1) three
  where 
    hasMax (_, b) = b == max
    max = maximum1Of (threeFold . _2) three

maxKeys2 f g three = toListOf (f . filtered hasMax . _1) three
  where 
    hasMax (_, b) = b == max
    max = maximum1Of (g . _2) three

--maxBid :: (forall a b. (Apply f) => (a -> f b) -> Three a -> f (Three b)) -> Int
--maxBid g = maximum1Of g (playerBidSizes g)

--maxBid2 f = maximum1Of (f . _2) (over (f . _2) (length ) playerCards :: Three (Int, Int))
--maxBidders  = toListOf (threeFold . filtered (\t -> snd t == maxBid) . _1)  playerBidSizes 

mkpair1 :: a -> b -> (a,b)
mkpair1 aa bb = (ida aa, bb)
    where
      ida :: a -> a -- This refers to a in the function's type signature
      ida = id

sortByIndex :: (Eq a) => [a] -> [a] -> [a]
sortByIndex order = sortOn $ flip elemIndex order

data SortOn a b = SortOn 
  { sortedOn :: a
  , originalValue :: b
  }

instance Eq a => Eq (SortOn a b) where
  SortOn l _ == SortOn r _ = l == r
instance Ord a => Ord ( SortOn a b ) where
  SortOn l _ <= SortOn r _ = l <= r

sortOnObj :: (a -> b) -> a -> SortOn b a 
sortOnObj f a = SortOn (f a) a

sortOnIndex :: (Ord a, Traversable t) =>  [a] -> t a -> t a
sortOnIndex order = fmap originalValue . sortTraversable . fmap (sortOnObj elemIndex') where
  elemIndex' a = elemIndex a order


