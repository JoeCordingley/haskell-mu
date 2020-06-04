{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Mu.CardPlay where

import           Cards                      (Card (..), ChiefTrump (..), Suit,
                                             Trump (..))
import           Control.Lens
import           Control.Monad              (replicateM)
import           Control.Monad.Reader (MonadReader, ask, asks)

import           Control.Monad.State.Lazy
import           Data.Foldable              (fold)
import           Data.Foldable              (toList)
import           Data.Function.Syntax
import           Data.Functor.Apply
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Mu.Auction                 (CardPositions (..), Chief (..),
                                             ViceTrump (..))
import           Mu.Players
import           Util                       (remove)

newtype NumberOfRounds =
  NumberOfRounds Int

type PlayableCard = (Card, CardPosition)

data CardPosition
  = InHand
  | OnTable
  deriving (Eq)

playableCards :: CardPositions -> [PlayableCard]
playableCards c = (fmap (, InHand) (inHand c)) <> (fmap (, OnTable) (onTable c))

playCardsStateful ::
     ( Monoid (players [Card])
     , Monoid (players ())
     , Monad m
     , Traversable1 players
     , Cycling player
     )
  => (forall c. player -> Lens' (players c) c)
  -> (player -> [PlayableCard] -> m PlayableCard)
  -> NumberOfRounds
  -> ChiefTrump
  -> Maybe ViceTrump
  -> Chief player
  -> players CardPositions
  -> m (players [Card])
playCardsStateful l getCardFromAvailable numberOfRounds chiefTrump viceTrump =
  fmap playableCards -.* evalStateT . playCards' 
  where
    getCard player cards =
      mapFst fst . removeFrom l player cards <$>
      getCardFromAvailable player (view (l player) cards)
    playCards' = playCards (playTrick l (StateT . getCard) chiefTrump viceTrump) numberOfRounds 


playCardsStatefulWithUpdate ::
     ( Monoid (players [Card])
     , Monoid (players ())
     , Monad m
     , Traversable1 players
     , Cycling player
     )
  => (WinnerOfTrick player -> m ())
  -> (forall c. player -> Lens' (players c) c)
  -> (player -> [PlayableCard] -> m PlayableCard)
  -> NumberOfRounds
  -> ChiefTrump
  -> Maybe ViceTrump
  -> Chief player
  -> players CardPositions
  -> m (players [Card])
playCardsStatefulWithUpdate update l getCardFromAvailable numberOfRounds chiefTrump viceTrump=
  fmap playableCards -.* evalStateT . playCards'  where
    getCard player cards =
      mapFst fst . removeFrom l player cards <$>
      getCardFromAvailable player (view (l player) cards)
    playCards' = playCards (playTrickAndUpdate (lift . update) l (StateT . getCard) chiefTrump viceTrump) numberOfRounds 

removeFrom
  :: Eq a =>
     (t -> ASetter' s [a] )
     -> t -> s -> a -> (a, s)
removeFrom l player cards card =
  (card, over (l player) (remove card) cards)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)


playCards
  :: (Monoid (players [Card]), Monad m) =>
     (s -> m (WonTrick players s))
     -> NumberOfRounds -> Chief s -> m (players [Card])
playCards playTrick (NumberOfRounds numberOfRounds) (Chief firstPlayer) = 
  evalStateT roundState firstPlayer
  where
    roundState = fold <$> replicateM numberOfRounds trickState
    trickState = StateT $ fmap (fmap getWinner . getWonTrick) . playTrick

playTrickAndUpdate
  :: (Monad m, Traversable1 players,
      Monoid (players [Card]), Monoid (players ()), Cycling player) =>
     (WinnerOfTrick player -> m a)
     -> (player -> ASetter' (players [Card]) [Card])
     -> (player -> m Card)
     -> ChiefTrump
     -> Maybe ViceTrump
     -> player
     -> m (WonTrick players player)
playTrickAndUpdate update l getCard chiefTrump viceTrump player = do
  wonTrick <- playTrick l getCard chiefTrump viceTrump player
  update . snd $ getWonTrick wonTrick
  return wonTrick

newtype WinnerOfTrick player = WinnerOfTrick 
  { getWinner :: player }
  

newtype WonTrick players player = WonTrick 
  { getWonTrick :: (players [Card], WinnerOfTrick player)
  }

playTrick ::
     ( Traversable1 players
     , Monoid (players [Card])
     , Monoid (players ())
     , Cycling player
     , Applicative m
     )
  => (player -> ASetter' (players [Card]) [Card])
  -> (player -> m Card)
  -> ChiefTrump
  -> Maybe ViceTrump
  -> player
  -> m (WonTrick players player)
playTrick l getCard chiefTrump viceTrump firstPlayer =
  wonTrick l chiefTrump viceTrump <$> traverse (withInput getCard) players
  where
    players = playersStartingFrom firstPlayer


playersStartingFrom ::
     (Traversable f, Monoid (f ()), Cycling player) => player -> f player
playersStartingFrom = evalState $ traverseEmpty players

wonTrick ::
     (Functor players, Foldable1 players, Monoid (players [Card]))
  => (player -> ASetter' (players [Card]) [Card])
  -> ChiefTrump
  -> Maybe ViceTrump
  -> players (player, Card)
  -> WonTrick players player
wonTrick l chiefTrump viceTrump playerCards = WonTrick (wonCards, WinnerOfTrick(winner))
  where
    wonCards = set (l winner) (toList cards) mempty
    cards = fmap snd $ playerCards
    ledSuit = LedSuit . suit $ head1 cards
    winner = winningPlayer $ foldMap1 trickWinner playerCards
    trickWinner (player, card) =
      TrickWinner player (trickCard chiefTrump viceTrump ledSuit card)

data CardPlayDeps players player = CardPlayDeps 
  { plens :: forall c. player -> Lens' (players c) c 
  , chiefTrump :: ChiefTrump
  , viceTrump :: Maybe ViceTrump
  }

wonTrick2 ::
     (Functor players, Foldable1 players, Monoid (players [Card]), MonadReader (CardPlayDeps players player) m)
  => players (player, Card)
  -> m (WonTrick players player)
wonTrick2 playerCards = fmap (flip wonTrick playerCards) ask
  where
    wonTrick (CardPlayDeps l chiefTrump viceTrump) playerCards = WonTrick (wonCards, WinnerOfTrick(winner)) where
      wonCards = set (l winner) (toList cards) mempty
      cards = fmap snd $ playerCards
      ledSuit = LedSuit . suit $ head1 cards
      winner = winningPlayer $ foldMap1 trickWinner playerCards
      trickWinner (player, card) =
        TrickWinner player (trickCard chiefTrump viceTrump ledSuit card)

head1 :: Foldable1 f => f a -> a
head1 = getFirst . foldMap1 First

newtype LedSuit =
  LedSuit Suit

trickCard :: ChiefTrump -> Maybe ViceTrump -> LedSuit -> Card -> TrickCard
trickCard (ChiefTrump chiefTrump) viceTrump (LedSuit ledSuit) card
  | isHighestTrump && isLowerTrump = TrumpCard DoublyTrump
  | isHighestTrump = TrumpCard (HighestTrump rank')
  | isLowerTrump = TrumpCard (LowerTrump rank')
  | isOfLedSuit = CardOfLedSuit rank'
  | otherwise = OtherCard
  where
    isHighestTrump = isTrump chiefTrump card
    isLowerTrump = any (\(ViceTrump trump) -> isTrump trump card) viceTrump
    isOfLedSuit = suit card == ledSuit
    rank' = Rank (rank card)

isTrump :: Trump -> Card -> Bool
isTrump (SuitTrump trumpSuit) (Card cardSuit _) = trumpSuit == cardSuit
isTrump (RankTrump trumpRank) (Card _ cardRank) = trumpRank == cardRank
isTrump NoTrump _                               = False

newtype Rank =
  Rank Int
  deriving (Eq, Ord)

data TrumpCard
  = LowerTrump Rank
  | HighestTrump Rank
  | DoublyTrump
  deriving (Eq, Ord)

data TrickCard
  = OtherCard
  | CardOfLedSuit Rank
  | TrumpCard TrumpCard
  deriving (Eq, Ord)

data TrickWinner player =
  TrickWinner
    { winningPlayer :: player
    , winningCard   :: TrickCard
    }

instance Semigroup (TrickWinner player) where
  TrickWinner earlierPlayer earlierCard <> TrickWinner laterPlayer laterCard =
    if earlierCard >= laterCard
      then TrickWinner earlierPlayer earlierCard
      else TrickWinner laterPlayer laterCard

withInput :: Functor f => (a -> f b) -> a -> f (a, b)
withInput f a = (a, ) <$> f a

traverseEmpty :: (Monoid (t ()), Traversable t, Applicative f) => f b -> f (t b)
traverseEmpty fb = traverse f mempty
  where
    f () = fb
