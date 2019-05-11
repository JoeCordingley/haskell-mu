module CardPlay
  ( TrickState(..)
  , PlayableCard(..)
  ) where

import           Cards
import           Control.Monad.State.Lazy
import qualified Data.Map.Lazy            as Map
import           Util
import AuctionPlay

data TrickState player = TrickState
  { cardsInHand  :: Map.Map player [Card]
  , cardsOnTable :: Map.Map player [Card]
  , cardLed      :: SuitLed
  }

data PlayableCard
  = CardOnTable Card
  | CardInHand Card

data EffectiveSuit = TrumpSuit | NormalSuit Suit deriving Eq

trumpsOf :: Trumps -> [Trump]
trumpsOf (SingleTrump trump) = [trump]
trumpsOf (HigherLower higher lower) = [higher, lower]

isTrump :: Trump -> Card -> Bool
isTrump (SuitTrump trumpSuit) (Card cardSuit _) = trumpSuit == cardSuit
isTrump (RankTrump trumpRank) (Card _ cardRank) = trumpRank == cardRank
isTrump NoTrump _ = False

effectiveSuit :: Trumps -> Card -> EffectiveSuit
effectiveSuit trumps card = if any (flip isTrump card) (trumpsOf trumps) then TrumpSuit else NormalSuit (suit card)

playableCards :: Ord player => player -> TrickState player -> [PlayableCard]
playableCards player state =
  playerCards CardOnTable cardsOnTable ++ playerCards CardInHand cardsInHand
  where
    playerCards playable cards =
      map playable . findOrEmptyList player $ cards state

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)


allowedCards ::
     Ord player => Trumps -> SuitLed -> player -> TrickState player -> [PlayableCard]
allowedCards trumps NewTrick = playableCards
allowedCards trumps (SuitLed suit) = allowed .: playableCards where
  allowed cards = case cardsOfLedSuit cards of
    [] -> cards
    allowedCards -> allowedCards
  cardsOfLedSuit cards = (filter isSuitLed cards ) :: [PlayableCard]
  isSuitLed card =  (effectiveSuit trumps $ cardOf card) == suit

data SuitLed = NewTrick | SuitLed EffectiveSuit

playCard ::
     (Monad f, Ord player)
  => (player -> [PlayableCard] -> f PlayableCard)
  -> Trumps
  -> player
  -> StateT (TrickState player) f Card
playCard getCard trumps player = do
  cardLed <- gets $ cardLed
  cards <- gets $ allowedCards trumps cardLed player
  card <- lift $ getCard player cards
  modify $ removeCard player card
  return $ cardOf card

cardOf :: PlayableCard -> Card
cardOf (CardOnTable card) = card
cardOf (CardInHand card)  = card

removeCard ::
     (Ord player)
  => player
  -> PlayableCard
  -> TrickState player
  -> TrickState player
removeCard player (CardOnTable card) state =
  state
    {cardsOnTable = Map.insertWith minus player [card] . cardsOnTable $ state}
removeCard player (CardInHand card) state =
  state {cardsInHand = Map.insertWith minus player [card] . cardsInHand $ state}

playTrick ::
     (Monad f, Ord player)
  => (player -> [PlayableCard] -> f PlayableCard)
  -> Trumps
  -> [player]
  -> StateT (TrickState player) f [Card]
playTrick getCard trumps = traverse (playCard getCard trumps)
