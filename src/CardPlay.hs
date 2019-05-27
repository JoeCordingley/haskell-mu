module CardPlay
  ( PlayableCard(..)
  , EffectiveSuit
  ) where

import           AuctionPlay
import           Cards
import           Control.Monad.State.Lazy
import           Data.List
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map
import           TrickWinner
import           Util

data CardPlayState player = CardPlayState
  { playerOrder   :: NonEmpty player
  , cardPositions :: CardPositions player
  }

data CardPositions player = CardPositions
  { cardsInHand  :: Map player [Card]
  , cardsOnTable :: Map player [Card]
  }

data PlayableCard
  = CardOnTable Card
  | CardInHand Card

playableCards :: Ord player => player -> CardPositions player -> [PlayableCard]
playableCards player state =
  playerCards CardOnTable cardsOnTable ++ playerCards CardInHand cardsInHand
  where
    playerCards playable cards =
      map playable . findOrEmptyList player $ cards state

allowedCards ::
     Ord player
  => Trumps
  -> SuitLed
  -> player
  -> CardPositions player
  -> [PlayableCard]
allowedCards trumps NewTrick = playableCards
allowedCards trumps (SuitLed suit) = allowed .: playableCards
  where
    allowed cards =
      case cardsOfLedSuit cards of
        []           -> cards
        allowedCards -> allowedCards
    cardsOfLedSuit cards = (filter isSuitLed cards) :: [PlayableCard]
    isSuitLed card = (effectiveSuit trumps $ cardOf card) == suit

data SuitLed
  = NewTrick
  | SuitLed EffectiveSuit

playCard ::
     (Monad f, Ord player)
  => (player -> [PlayableCard] -> f PlayableCard)
  -> Trumps
  -> CardPositions player
  -> SuitLed
  -> player
  -> f PlayableCard
playCard getCard trumps cardPositions suitLed player =
  getCard player $ allowedCards trumps suitLed player cardPositions

cardOf :: PlayableCard -> Card
cardOf (CardOnTable card) = card
cardOf (CardInHand card)  = card

playTrick ::
     (Monad f, Ord player)
  => (player -> [PlayableCard] -> f PlayableCard)
  -> Trumps
  -> StateT (CardPlayState player) f (player, NonEmpty Card)
playTrick getCard trumps = StateT playTrick'
  where
    playTrick' state = do
      firstCard <- playCard' NewTrick firstPlayer
      rest <- traverse (playCard' $ suitLedFrom firstCard) laterPlayers
      let cards = firstCard :| rest
          cardsAsPlayed = players `NE.zip` cards
          playedTrick = players `NE.zip` trick
          trick = NE.map cardOf cards
          winner' = winner trumps playedTrick
          wonTrick = (winner', trick)
          newState = updateState winner' (NE.toList cardsAsPlayed) state
      return (wonTrick, newState)
      where
        players = playerOrder state
        firstPlayer = NE.head players
        suitLedFrom = SuitLed . effectiveSuit trumps . cardOf
        laterPlayers = NE.tail players
        playCard' = playCard getCard trumps $ cardPositions state

playRounds ::
     (Monad f, Ord player)
  => (player -> [PlayableCard] -> f PlayableCard)
  -> Int
  -> Trumps
  -> CardPlayState player
  -> f (Map player [Card])
playRounds getCard numberOfRounds trumps initialState =
  accum <$> evalStateT
    (traverse (\_ -> playTrick getCard trumps) [1 .. numberOfRounds])
    initialState

accum :: Ord player => [(player, NonEmpty Card)] -> Map player [Card]
accum = foldr addCards Map.empty where
  addCards (player, cards) = Map.insertWith (++) player $ NE.toList cards

traverseAndFoldr :: (Foldable t, Applicative f) => (b -> c -> c) -> (a -> f b) -> c -> t a -> f c
traverseAndFoldr f g z = foldr f' $ pure z where f' a fc = f <$> g a <*> fc


updateState ::
     Ord player
  => player
  -> [(player, PlayableCard)]
  -> CardPlayState player
  -> CardPlayState player
updateState winner playerCards (CardPlayState playerOrder cardPositions) =
  CardPlayState
    { playerOrder = newOrder winner playerOrder
    , cardPositions = newPositions playerCards cardPositions
    }

newOrder :: Eq player => player -> NonEmpty player -> NonEmpty player
newOrder winner' playerOrder' = playersFromWinner
  where
    players = NE.toList playerOrder'
    numberOfPlayers = length players
    playersFromWinner =
      NE.fromList . take numberOfPlayers . dropWhile (/= winner') $
      cycle players

newPositions ::
     Ord player
  => [(player, PlayableCard)]
  -> CardPositions player
  -> CardPositions player
newPositions playedCards CardPositions { cardsInHand = cardsInHand
                                       , cardsOnTable = cardsOnTable
                                       } =
  CardPositions {cardsInHand = newCardsInHand, cardsOnTable = newCardsOnTable}
  where
    newCardsInHand = foldr removeInHand cardsInHand playedCards
    newCardsOnTable = foldr removeOnTable cardsOnTable playedCards
    removeInHand (player, (CardInHand card)) =
      Map.insertWith minus player [card]
    removeOnTable (player, (CardOnTable card)) =
      Map.insertWith minus player [card]
