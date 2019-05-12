module TrickWinner
  ( EffectiveSuit(..)
  , effectiveSuit
  , winner
  ) where

import Cards
import AuctionPlay
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NE
import           Data.List
import Util

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
