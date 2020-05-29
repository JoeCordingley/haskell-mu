{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module New.CardPlay where

import AuctionFunctions (Chief(..))
import Cards (Card(..), Trumps(..), Suit, Trump(..))
import New.Bidding (CardPositions)
import Data.Foldable (fold)
import Control.Monad (replicateM)
import Data.Functor.Apply 
import Control.Monad.State.Lazy
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Semigroup
import New.Players
import Control.Lens
import Data.Foldable (toList)
import Data.Composition 
import Util (remove)


newtype NumberOfRounds = NumberOfRounds Int 

data PlayableCard 
  = InHand Card
  | OnTable Card deriving Eq

getPlayableCard :: PlayableCard -> Card
getPlayableCard (InHand card) = card
getPlayableCard (OnTable card) = card

playableCards :: CardPositions -> [PlayableCard]
playableCards = undefined

playCardsStateful :: (Monoid (players [Card]), Monoid (players ()), Monad m, Traversable1 players, Cycling player) 
  => (forall c. player -> Lens' (players c) c)
  -> (player -> [PlayableCard] -> m PlayableCard) 
  -> NumberOfRounds 
  -> Trumps 
  -> Chief player 
  -> players CardPositions
  -> m (players [Card])
playCardsStateful l getCardFromAvailable n t c = evalStateT (playCards l (StateT . getCard) n t c) . fmap playableCards  where
  getCard player cards = removeFrom player cards <$> getCardFromAvailable player (view (l player) cards)
  removeFrom player cards card = (getPlayableCard card, over (l player) (remove card) cards)

playCards :: (Monoid (players [Card]), Monoid (players ()), Monad m, Traversable1 players, Cycling player) 
  => (player -> ASetter' (players [Card]) [Card])
  -> (player -> m Card) 
  -> NumberOfRounds 
  -> Trumps 
  -> Chief player 
  ->  m (players [Card])
playCards l getCard (NumberOfRounds numberOfRounds) trumps (Chief firstPlayer) = evalStateT roundState firstPlayer where
  roundState = fold <$> replicateM numberOfRounds trickState
  trickState = StateT $ playTrick l getCard trumps

playTrick :: (Traversable1 players, Monoid (players [Card]), Monoid (players ()), Cycling player, Applicative m) 
  => (player -> ASetter' (players [Card]) [Card])
  -> (player -> m Card)
  -> Trumps
  -> player
  -> m (players [Card], player)
playTrick l getCard trumps firstPlayer = cardsAndWinner l trumps <$> traverse (withInput getCard) players where
  players = playersStartingFrom firstPlayer

playersStartingFrom :: (Traversable f,  Monoid (f ()), Cycling player) => player -> f player
playersStartingFrom = evalState $ traverseEmpty players

cardsAndWinner
  :: (Functor t, Foldable1 t, Monoid (t [Card])) =>
     (player -> ASetter' (t [Card]) [Card])
     -> Trumps -> t (player, Card) -> (t [Card], player)
cardsAndWinner l trumps playerCards = (wonCards, winner) where
  wonCards = set (l winner) (toList cards) mempty
  cards = fmap snd $ playerCards
  ledSuit = LedSuit . suit $ head1 cards
  winner = winningPlayer $ foldMap1 trickWinner playerCards
  trickWinner (player, card) = TrickWinner player (trickCard trumps ledSuit card)

head1 :: Foldable1 f => f a -> a
head1 = getFirst . foldMap1 First

newtype LedSuit = LedSuit Suit

trickCard :: Trumps -> LedSuit -> Card -> TrickCard
trickCard trumps (LedSuit ledSuit) card = 
  if isHighestTrump && isLowerTrump then TrumpCard DoublyTrump
  else if isHighestTrump then TrumpCard (HighestTrump rank')
  else if isLowerTrump then TrumpCard (LowerTrump rank')
  else if isOfLedSuit then CardOfLedSuit rank'
  else OtherCard where
    isHighestTrump = isTrump (highestTrump trumps) card
    isLowerTrump = any (flip isTrump card) (lowerTrump trumps)
    isOfLedSuit = suit card == ledSuit
    rank' = Rank (rank card)

highestTrump :: Trumps -> Trump
highestTrump (SingleTrump trump) = trump
highestTrump (HigherLower higher _ ) = higher

lowerTrump :: Trumps -> Maybe Trump
lowerTrump (SingleTrump _) = Nothing
lowerTrump (HigherLower _ lower) = Just lower

isTrump :: Trump -> Card -> Bool
isTrump (SuitTrump trumpSuit) (Card cardSuit _) = trumpSuit == cardSuit
isTrump (RankTrump trumpRank) (Card _ cardRank) = trumpRank == cardRank
isTrump NoTrump _                               = False

newtype Rank = Rank Int deriving (Eq, Ord)

data TrumpCard 
  = LowerTrump Rank
  | HighestTrump Rank
  | DoublyTrump deriving (Eq, Ord)

data TrickCard 
  = OtherCard
  | CardOfLedSuit Rank
  | TrumpCard TrumpCard deriving (Eq, Ord)
  
data TrickWinner player = TrickWinner
  { winningPlayer :: player 
  , winningCard :: TrickCard
  }

instance Semigroup (TrickWinner player) where
  TrickWinner earlierPlayer earlierCard <> TrickWinner laterPlayer laterCard = 
    if earlierCard >= laterCard then TrickWinner earlierPlayer earlierCard else TrickWinner laterPlayer laterCard
  
withInput :: Functor f => (a -> f b) -> a -> f (a,b)
withInput f a = (a,) <$> f a

traverseEmpty :: (Monoid (t ()), Traversable t, Applicative f) => f b -> f (t b)
traverseEmpty fb = traverse f mempty where
  f () = fb
