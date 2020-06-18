{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}

module Mu.Auction where

import           Cards
import           Control.Lens              hiding ((<.>))
import           Control.Monad.State.Class
import           Control.Monad.State.Lazy
import           Data.Foldable             (toList)
import           Data.Function.Syntax
import           Data.Functor.Apply
import           Data.Functor.Compose
import           Data.List                 hiding (sortOn)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE
import           Data.Monoid
import           Data.Semigroup            (Max (..), getMax)
import qualified Data.Semigroup            as Semi
import           Data.Semigroup.Foldable
import           Data.Tuple.Homogenous
import           Mu.Players
import           TupleInstances
import           Util

newtype CardsBid =
  CardsBid Int
  deriving (Eq, Ord, Show, Num, Enum)

data Bid
  = Pass
  | Raise [Card]
  deriving (Show, Eq)

newtype Chief player =
  Chief
    { getChief :: player
    }
  deriving (Eq, Show)

data BiddingResult player
  = SuccessfulBiddingResult (WinnersAndTopBid player)
  | UnsuccessfulBiddingResult (Stalemate player)

viceOrdering :: [Card] -> [Card] -> Ordering
viceOrdering = compareBy viceComparisons

compareBy :: Traversable t => t (a -> b -> Ordering) -> a -> b -> Ordering
compareBy fs a b =
  case find (/= EQ) $ fmap (\c -> c a b) fs of
    Just unequal -> unequal
    Nothing      -> EQ

viceComparisons :: [[Card] -> [Card] -> Ordering]
viceComparisons = compareLength : map compareNs [9,8 .. 1]

compareNs :: Rank -> [Card] -> [Card] -> Ordering
compareNs n as bs = compare (numberOfNs as) (numberOfNs bs)
  where
    numberOfNs = length . filter (== n) . map rank

compareLength :: [a] -> [b] -> Ordering
compareLength as bs = compare (length as) (length bs)

data WinnersAndTopBid player =
  WinnersAndTopBid (Chief player) (Maybe (Vice player)) CardsBid

data Stalemate player
  = EklatNoPoints
  | Eklat
      { topBid   :: CardsBid
      , atFault  :: player
      , affected :: NonEmpty player
      }
  deriving (Eq, Show)

data FinishedBidding players player
  = Successful (SuccessfulBidding players player)
  | Unsuccessful (Stalemate player)

newtype Vice player =
  Vice
    { getVice :: player
    }

data SuccessfulBidding players player =
  SuccessfulBidding
    { chief            :: Chief player
    , vice             :: Maybe (Vice player)
    , successfulTopBid :: CardsBid
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
finishBidding (UnsuccessfulBiddingResult stalemate, _) = Unsuccessful stalemate

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

evalStatePair s (a, b) = evalStateT (evalStateT s a) b

tallyAuction ::
     (Eq player, Foldable1 players)
  => players (player, [Card])
  -> BiddingResult player
tallyAuction playerCards =
  if topBid == CardsBid (0)
    then UnsuccessfulBiddingResult (EklatNoPoints)
    else case NE.reverse maxBidders' of
           chief :| [] ->
             SuccessfulBiddingResult (successful (Chief chief) topBid)
           lastPlayerToRaise :| penultimate:others ->
             UnsuccessfulBiddingResult
               (Eklat topBid lastPlayerToRaise (penultimate :| others))
  where
    MaxBidders (Semi.Max topBid) maxBidders' = foldMap1 playerBids playerCards
    successful chief = WinnersAndTopBid chief (vice chief)
    vice chief =
      case vices chief of
        [vice] -> (Just (Vice vice))
        _      -> Nothing
    vices (Chief chief) =
      maxBidders . foldMap viceBids . NE.filter (notPlayer chief) $
      toNonEmpty playerCards
    notPlayer player = (/= player) . fst
    viceBids (player, cards) = MaxBidders (ViceBid cards) [player]
    playerBids (player, cards) =
      MaxBidders (Semi.Max (CardsBid (length cards))) (player :| [])

newtype MaxRaise =
  MaxRaise Int

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

playAuctionAndUpdate ::
     ( Foldable1 players
     , Traversable players
     , Eq player
     , Monad f
     , Monoid (players [Card])
     , Cycling player
     , Applicative players
     )
  => (BiddingResult player -> f ())
  -> (player -> f Bid)
  -> (player -> [Card] -> players [Card])
  -> players player
  -> player
  -> f (BiddingResult player)
playAuctionAndUpdate update = update' .*** playAuction
  where
    update' fResult = do
      result <- fResult
      update result
      return result

playAuctionAndRecord ::
     ( Foldable1 players
     , Traversable players
     , Eq player
     , Monad f
     , Monoid (players [Card])
     , Cycling player
     , Applicative players
     , Functor players
     )
  => players player
  -> (forall c. player -> Lens' (players c) c)
  -> (player -> MaxRaise -> [Card] -> f Bid)
  -> player
  -> players [Card]
  -> f (FinishedBidding players player)
playAuctionAndRecord = playAuctionAndRecordWithUpdate doNothing
  where
    doNothing _ = return ()

playAuctionAndRecordWithUpdate ::
     ( Foldable1 players
     , Traversable players
     , Eq player
     , Monad f
     , Monoid (players [Card])
     , Cycling player
     , Applicative players
     , Functor players
     )
  => (BiddingResult player -> f ())
  -> players player
  -> (forall c. player -> Lens' (players c) c)
  -> (player -> MaxRaise -> [Card] -> f Bid)
  -> player
  -> players [Card]
  -> f (FinishedBidding players player)
playAuctionAndRecordWithUpdate updateResult players l getBid firstPlayer =
  fmap finishBidding .
  runStateT
    (playAuctionAndUpdate
       (lift . updateResult)
       (StateT . getBidStateful getBid l)
       raise
       players
       firstPlayer) .
  fmap initialPositions
  where
    raise player cards = set (l player) cards mempty

getBidStateful ::
     (Functor f, Foldable1 players)
  => (player -> MaxRaise -> [Card] -> f Bid)
  -> (player -> LensLike' (Compose f ((,) Bid)) (players CardPositions) CardPositions)
  -> player
  -> (players CardPositions)
  -> f (Bid, (players CardPositions))
getBidStateful getBid l player players =
  getCompose $ (l player) (Compose . getBidAndUpdate player) players
  where
    highestBid = getMax $ foldMap1 (Max . length . onTable) players
    getBidAndUpdate player cardPositions =
      update cardPositions <$> getBid player maxRaise cards
      where
        cards = inHand cardPositions
        maxRaise = MaxRaise $ maxBid - currentBid
        maxBid = highestBid + 1
        currentBid = length $ onTable cardPositions
    update CardPositions {inHand, onTable} (Raise cards) =
      ( Raise cards
      , CardPositions {inHand = minus cards inHand, onTable = cards <> onTable})
    update cardPositions Pass = (Pass, cardPositions)

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

newtype IsMoreThanThreePlayers =
  IsMoreThanThreePlayers Bool

newtype ViceTrump =
  ViceTrump
    { getViceTrump :: Trump
    }

newtype Partner player =
  Partner player
  deriving (Eq)

data TrumpsAndPartner player =
  TrumpsAndPartner
    { chiefTrump :: ChiefTrump
    , viceTrump  :: Maybe ViceTrump
    , partner    :: Maybe (Partner player)
    }

settleAuctionRound ::
     Monad m
  => IsMoreThanThreePlayers
  -> (vice -> [Trump] -> m ViceTrump)
  -> (chief -> [Trump] -> m ChiefTrump)
  -> (chief -> players -> m (Partner player))
  -> players
  -> (chief, [Card])
  -> Maybe (vice, [Card])
  -> m (TrumpsAndPartner player)
settleAuctionRound (IsMoreThanThreePlayers isMoreThanThreePlayers) getViceTrump getChiefTrump getPartner players (chief, chiefCards) viceAndCards = do
  viceTrump <-
    traverse (uncurry getViceTrump . fmap getViceTrumpOptions) viceAndCards
  chiefTrump <- getChiefTrump chief $ getChiefTrumpOptions chiefCards
  partner <-
    if isMoreThanThreePlayers
      then fmap Just (getPartner chief players)
      else return Nothing
  return TrumpsAndPartner {chiefTrump, viceTrump, partner}

getViceTrumpOptions :: [Card] -> [Trump]
getViceTrumpOptions cards = nub trumps
  where
    trumps = do
      card <- cards
      [SuitTrump $ suit card, RankTrump $ rank card]

getChiefTrumpOptions :: [Card] -> [Trump]
getChiefTrumpOptions = (NoTrump :) . getViceTrumpOptions

toBid :: [Card] -> Bid
toBid []    = Pass
toBid cards = Raise cards

newtype ChiefTrump =
  ChiefTrump Trump
