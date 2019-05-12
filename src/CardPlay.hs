module CardPlay
  ( PlayableCard(..)
  ) where

import           AuctionPlay
import           Cards
import           Control.Monad.State.Lazy
import           Data.List
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Data.Map.Lazy            (Map)
import qualified Data.Map.Lazy            as Map
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

data EffectiveSuit
  = TrumpSuit
  | NormalSuit Suit
  deriving (Eq)

trumpsOf :: Trumps -> [Trump]
trumpsOf (SingleTrump trump)        = [trump]
trumpsOf (HigherLower higher lower) = [higher, lower]

isTrump :: Trump -> Card -> Bool
isTrump (SuitTrump trumpSuit) (Card cardSuit _) = trumpSuit == cardSuit
isTrump (RankTrump trumpRank) (Card _ cardRank) = trumpRank == cardRank
isTrump NoTrump _                               = False

effectiveSuit :: Trumps -> Card -> EffectiveSuit
effectiveSuit trumps card =
  if any (flip isTrump card) (trumpsOf trumps)
    then TrumpSuit
    else NormalSuit (suit card)

playableCards :: Ord player => player -> CardPositions player -> [PlayableCard]
playableCards player state =
  playerCards CardOnTable cardsOnTable ++ playerCards CardInHand cardsInHand
  where
    playerCards playable cards =
      map playable . findOrEmptyList player $ cards state

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

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
  -> f [(player, NonEmpty Card)]
playRounds getCard numberOfRounds trumps initialState =
  evalStateT
    (traverse (\_ -> playTrick getCard trumps) [1 .. numberOfRounds])
    initialState

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

winner :: Trumps -> NonEmpty (player, Card) -> player
winner trumps playerCards = fst $ highestEarliest comparison playerCards
  where
    comparison leftPair rightPair =
      compareCards trumps suitLed (snd leftPair) (snd rightPair)
    suitLed = effectiveSuit trumps . snd $ NE.head playerCards

highestEarliest :: (a -> a -> Ordering) -> NonEmpty a -> a
highestEarliest f (a :| as) = foldl g a as
  where
    g a a' =
      case f a a' of
        LT -> a'
        EQ -> a
        GT -> a

compareCards :: Trumps -> EffectiveSuit -> Card -> Card -> Ordering
compareCards trumps suitLed = firstUnequal .: comparisons trumps suitLed

firstUnequal :: [Ordering] -> Ordering
firstUnequal orderings =
  case find isUnequal orderings of
    Just unequal -> unequal
    Nothing      -> EQ
  where
    isUnequal EQ = False
    isUnequal _  = True

comparisons :: Trumps -> EffectiveSuit -> Card -> Card -> [Ordering]
comparisons trumps@(SingleTrump trump) suitLed leftCard rightCard =
  map
    (\c -> leftCard `c` rightCard)
    [compareTrump trump, isSuitLed trumps suitLed, compareRank]
comparisons trumps@(HigherLower higher lower) suitLed leftCard rightCard =
  map
    (\c -> leftCard `c` rightCard)
    [ compareTrump higher
    , compareTrump lower
    , isSuitLed trumps suitLed
    , compareRank
    ]

compareRank :: Card -> Card -> Ordering
compareRank leftCard rightCard = rank leftCard `compare` rank rightCard

isSuitLed :: Trumps -> EffectiveSuit -> Card -> Card -> Ordering
isSuitLed trumps suitLed leftCard rightCard =
  (effectiveSuit trumps leftCard == suitLed) `compare`
  (effectiveSuit trumps rightCard == suitLed)

compareTrump :: Trump -> Card -> Card -> Ordering
compareTrump (SuitTrump suit') leftCard rightCard =
  (suit leftCard == suit') `compare` (suit rightCard == suit')
compareTrump (RankTrump rank') leftCard rightCard =
  (rank leftCard == rank') `compare` (rank rightCard == rank')
