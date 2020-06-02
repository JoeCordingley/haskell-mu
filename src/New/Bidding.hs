module New.Bidding where

import           AuctionFunctions          (Bid (..), Chief (..), Winners (..),
                                            viceOrdering)
import           Cards
import           Control.Lens              hiding ((<.>))
import           Control.Monad.State.Class
import           Control.Monad.State.Lazy
import           Data.Foldable             (toList)
import           Data.Functor.Apply
import           Data.List                 hiding (sortOn)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE
import           Data.Monoid
import qualified Data.Semigroup            as Semi
import           Data.Semigroup.Foldable
import           Data.Tuple.Homogenous
import           New.Players
import           New.TupleInstances
import           New.Util

data BiddingResult player
  = SuccessfulBiddingResult (WinnersAndTopBid player)
  | UnsuccessfulBiddingResult (Stalemate player)

data WinnersAndTopBid player =
  WinnersAndTopBid (Chief player) (Maybe (Vice player)) TopBid

data Stalemate player
  = EklatNoPoints
  | Eklat
      { topBid   :: TopBid
      , atFault  :: player
      , affected :: NonEmpty player
      }
  deriving (Eq, Show)

data FinishedBidding players player
  = Successful (SuccessfulBidding players player)
  | Unsuccessful (Stalemate player)

newtype TopBid = TopBid Int deriving (Show, Eq, Ord)

newtype Vice player = Vice player

data SuccessfulBidding players player =
  SuccessfulBidding
    { chief   :: Chief player
    , vice :: Maybe (Vice player)
    , successfulTopBid :: TopBid
    , biddingPositions :: players CardPositions
    }

data CardPositions =
  CardPositions
    { inHand  :: [Card]
    , onTable :: [Card]
    }

finishBidding ::
     (BiddingResult player, players CardPositions)
  -> FinishedBidding players player
finishBidding (SuccessfulBiddingResult (WinnersAndTopBid chief vice topBid), cardPositions) =
  Successful (SuccessfulBidding chief vice topBid cardPositions)

finishBidding2 ::
     Functor players
  => (BiddingResult player, players (player, CardPositions))
  -> FinishedBidding players player
finishBidding2 (SuccessfulBiddingResult (WinnersAndTopBid chief vice topBid), cardPositions) =
  Successful (SuccessfulBidding chief vice topBid (snd <$> cardPositions))

initialPositions :: [Card] -> CardPositions
initialPositions cards = CardPositions {inHand = cards, onTable = []}

type GetBid f player = player -> [Card] -> f Bid

runAuction ::
     (Monad f, Monoid bids, Cycling player)
  => (player -> f Bid)
  -> (player -> [Card] -> bids)
  -> Int
  -> player
  -> f (bids, [player])
runAuction getBid raise numberOfPlayers firstPlayer =
  evalStateT (runAuction' numberOfPlayers) firstPlayer
  where
    runAuction' 0 = return $ mempty
    runAuction' passesLeft = do
      player <- players
      bid <- lift $ getBid player
      case bid of
        Pass -> runAuction' (passesLeft - 1)
        Raise cards ->
          ((raise player cards, [player]) <>) <$> runAuction' numberOfPlayers

tallyAuction ::
     (Eq player, Foldable1 players)
  => players (player, [Card])
  -> BiddingResult player
tallyAuction playerCards =
  if topBid == TopBid(0)
    then UnsuccessfulBiddingResult (EklatNoPoints)
    else case NE.reverse maxBidders' of
           chief :| [] -> SuccessfulBiddingResult (successful (Chief chief) topBid)
           lastPlayerToRaise :| penultimate:others ->
             UnsuccessfulBiddingResult
               (Eklat topBid lastPlayerToRaise (penultimate :| others))
  where
    MaxBidders (Semi.Max topBid) maxBidders' = foldMap1 playerBids playerCards
    successful chief = WinnersAndTopBid chief (vice chief)
    vice chief =
      case vices chief of
        [vice] -> (Just (Vice vice))
        _      ->  Nothing
    vices (Chief chief) =
      maxBidders . foldMap viceBids . NE.filter (notPlayer chief) $
      toNonEmpty playerCards
    notPlayer player = (/= player) . fst
    viceBids (player, cards) = MaxBidders (ViceBid cards) [player]
    playerBids (player, cards) =
      MaxBidders (Semi.Max (TopBid (length cards))) (player :| [])

playAuction ::
     ( Foldable1 players
     , Traversable players
     , Eq player
     , Monad f
     , Monoid (players [Card])
     , Cycling player
     , Applicative players
     )
  => (player -> f Bid)
  -> (player -> [Card] -> players [Card])
  -> players player
  -> player
  -> f (BiddingResult player)
playAuction getBid raise players firstPlayer =
  tallyAuction . uncurry bidsByLastRaise . over _1 (playerCards players) <$>
  runAuction getBid raise n firstPlayer
  where
    n = length players

playAuctionAndRecord ::
     ( Foldable1 players1
     , Traversable players1
     , Eq player
     , Monad f
     , Monoid (players1 [Card])
     , Cycling player
     , Applicative players1
     )
  => (player -> StateT (players2 CardPositions) f Bid)
  -> (player -> [Card] -> players1 [Card])
  -> players1 player
  -> player
  -> players2 CardPositions
  -> f (FinishedBidding players2 player)
playAuctionAndRecord getBid raise players firstPlayer =
  fmap finishBidding . runStateT (playAuction getBid raise players firstPlayer)

bid :: [Card] -> Bid
bid []    = Pass
bid cards = Raise cards

playerCards players cards = (,) <$> players <*> cards

bidsByLastRaise ::
     (Eq player, Traversable f)
  => f (player, cards)
  -> [player]
  -> f (player, cards)
bidsByLastRaise bids lastRaised = sortOn playerIndex bids
  where
    playerIndex (player, _) = elemIndex player lastRaised

newtype ViceBid =
  ViceBid [Card]

instance Eq ViceBid where
  ViceBid a == ViceBid b = viceOrdering a b == EQ

instance Ord ViceBid where
  ViceBid a <= ViceBid b = viceOrdering a b /= GT

instance Zeroed ViceBid where
  zero = ViceBid []

data MaxBidders players bid =
  MaxBidders
    { maxBid     :: bid
    , maxBidders :: players
    }

instance (Ord bid, Semigroup players) =>
         Semigroup (MaxBidders players bid) where
  MaxBidders bidA playersA <> MaxBidders bidB playersB =
    case compare bidA bidB of
      GT -> MaxBidders bidA playersA
      LT -> MaxBidders bidB playersB
      EQ -> MaxBidders bidA (playersA <> playersB)

class Ord a =>
      Zeroed a
  where
  zero :: a

instance (Monoid players, Zeroed bid) => Monoid (MaxBidders players bid) where
  mempty = MaxBidders zero mempty
