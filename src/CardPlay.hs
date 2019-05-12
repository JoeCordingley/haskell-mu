module CardPlay
  ( TrickState(..)
  , PlayableCard(..)
  ) where

import           AuctionPlay
import           Cards
import           Control.Monad.State.Lazy
import           Data.List
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import qualified Data.Map.Lazy            as Map
import           Util

data TrickState player = TrickState
  { playerOrder  :: NonEmpty player
  , cardsInHand  :: Map.Map player [Card]
  , cardsOnTable :: Map.Map player [Card]
  , cardLed      :: SuitLed
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

playableCards :: Ord player => player -> TrickState player -> [PlayableCard]
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
  -> TrickState player
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
  -> player
  -> StateT (TrickState player) f Card
playCard getCard trumps player = do
  cardLed <- gets $ cardLed
  cards <- gets $ allowedCards trumps cardLed player
  card <- lift $ getCard player cards
  modify $ removeCard player card
  return $ cardOf card

playCard2 ::
     (Monad f, Ord player)
  => (player -> [PlayableCard] -> f PlayableCard)
  -> Trumps
  -> TrickState player
  -> SuitLed
  -> player
  -> f PlayableCard
playCard2 getCard trumps trickState suitLed player =
  getCard player $ allowedCards trumps suitLed player trickState

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
playTrick getCard trumps (first:later) = do
  firstCard <- playFirstCard getCard trumps first
  rest <- traverse (playCard getCard trumps) later
  return $ firstCard : rest

playTrick2 ::
     (Monad f, Ord player)
  => (player -> [PlayableCard] -> f PlayableCard)
  -> Trumps
  -> TrickState player
  -> f (TrickState player)
playTrick2 getCard trumps trickState = do
  firstCard <- playFirstCard
  rest <- playLaterCards firstCard
  return $ updateState' firstCard rest 
  where
    firstPlayer = NE.head $ playerOrder trickState
    laterPlayers = NE.tail $ playerOrder trickState
    playFirstCard = playCard2 getCard trumps trickState NewTrick firstPlayer
    playLaterCards firstCard = traverseWith (playCard2 getCard trumps trickState (SuitLed . effectiveSuit trumps $ cardOf firstCard)) laterPlayers
    playerCards firstCard rest = (firstPlayer, firstCard) :| rest
    --updateState' firstCard rest = updateState trumps (playerCards firstCard rest) trickState
    updateState' = flip (updateState trumps) trickState .: playerCards


updateState :: Trumps -> NonEmpty (player, PlayableCard) -> TrickState player -> TrickState player
updateState trumps playerCards = undefined


traverseWith ::
     (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t (a, b))
traverseWith f t = traverse (\a -> fmap (\b -> (a, b)) $ f a) t

--playTrick2 ::
--     (Monad f, Ord player)
--  => (player -> [PlayableCard] -> f PlayableCard)
--  -> Trumps
--  -> [player]
--  -> StateT (TrickState player) f [Card]
--playTrick2 getCard trumps (first:later)= StateT playTrick' where
--  playTrick' trickState = undefined
--  cards trickState = do
--    firstCard <- playCard2 getCard trumps trickState NewTrick first
--    rest <- traverse (playCard2 getCard trumps trickState (SuitLed . effectiveSuit trumps $ cardOf firstCard)) later
--    return $ firstCard:rest
playFirstCard ::
     (Monad f, Ord player)
  => (player -> [PlayableCard] -> f PlayableCard)
  -> Trumps
  -> player
  -> StateT (TrickState player) f Card
playFirstCard getCard trumps player = do
  card <- playCard getCard trumps player
  setCardLed trumps card
  return card

setCardLed :: (Monad f) => Trumps -> Card -> StateT (TrickState player) f ()
setCardLed trumps card = modify setCard
  where
    setCard state = state {cardLed = effective}
    effective = SuitLed $ effectiveSuit trumps card

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
