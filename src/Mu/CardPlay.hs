{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Mu.CardPlay where

import           Cards                      (Card (..), ChiefTrump (..), Suit,
                                             Trump (..))
import           Control.Lens
import           Control.Monad              (replicateM)
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
playCardsStateful l getCardFromAvailable =
  fmap playableCards -.**** evalStateT .*** playCards l (StateT . getCard)
  where
    getCard player cards =
      removeFrom player cards <$>
      getCardFromAvailable player (view (l player) cards)
    removeFrom player cards card =
      (fst card, over (l player) (remove card) cards)

playCards ::
     ( Monoid (players [Card])
     , Monoid (players ())
     , Monad m
     , Traversable1 players
     , Cycling player
     )
  => (player -> ASetter' (players [Card]) [Card])
  -> (player -> m Card)
  -> NumberOfRounds
  -> ChiefTrump
  -> Maybe ViceTrump
  -> Chief player
  -> m (players [Card])
playCards l getCard (NumberOfRounds numberOfRounds) chiefTrump viceTrump (Chief firstPlayer) =
  evalStateT roundState firstPlayer
  where
    roundState = fold <$> replicateM numberOfRounds trickState
    trickState = StateT $ playTrick l getCard chiefTrump viceTrump

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
  -> m (players [Card], player)
playTrick l getCard chiefTrump viceTrump firstPlayer =
  cardsAndWinner l chiefTrump viceTrump <$> traverse (withInput getCard) players
  where
    players = playersStartingFrom firstPlayer

playersStartingFrom ::
     (Traversable f, Monoid (f ()), Cycling player) => player -> f player
playersStartingFrom = evalState $ traverseEmpty players

cardsAndWinner ::
     (Functor t, Foldable1 t, Monoid (t [Card]))
  => (player -> ASetter' (t [Card]) [Card])
  -> ChiefTrump
  -> Maybe ViceTrump
  -> t (player, Card)
  -> (t [Card], player)
cardsAndWinner l chiefTrump viceTrump playerCards = (wonCards, winner)
  where
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
