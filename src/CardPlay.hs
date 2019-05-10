module CardPlay
  ( TrickState(..)
  , PlayableCard(..)
  ) where

import           Cards
import           Control.Monad.State.Lazy
import qualified Data.Map.Lazy            as Map
import           Util

data TrickState player = TrickState
  { cardsInHand  :: Map.Map player [Card]
  , cardsOnTable :: Map.Map player [Card]
  , cardLed      :: CardLed
  }

data PlayableCard
  = CardOnTable Card
  | CardInHand Card

playableCards :: Ord player => player -> TrickState player -> [PlayableCard]
playableCards player state =
  playerCards CardOnTable cardsOnTable ++ playerCards CardInHand cardsInHand
  where
    playerCards playable cards =
      map playable . findOrEmptyList player $ cards state

allowedCards ::
     Ord player => player -> CardLed -> TrickState player -> [PlayableCard]
allowedCards = undefined

data CardLed
  = NewTrick
  | CardLed Card

playCard ::
     (Monad f, Ord player)
  => (player -> [PlayableCard] -> f PlayableCard)
  -> player
  -> StateT (TrickState player) f Card
playCard getCard player = do
  cardLed <- gets $ cardLed
  cards <- gets $ allowedCards player cardLed
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
  -> [player]
  -> StateT (TrickState player) f [Card]
playTrick getCard = traverse (playCard getCard)
