{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cards where

import           Data.Monoid (Sum)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Rank = Int

data Suit
  = Red
  | Black
  | Blue
  | Yellow
  | Green
  deriving (Enum, Eq, Show, Ord)

data Card =
  Card
    { suit :: Suit
    , rank :: Rank
    }
  deriving (Eq, Show, Ord)

newtype Score =
  Score
    { getScore :: Int
    }
  deriving (Num, Eq, Ord)

instance Semigroup Score where
  Score l <> Score r = Score (l + r)

instance Monoid Score where
  mempty = Score 0

points :: Rank -> Score
points rank
  | elem rank [6, 7] = 2
  | elem rank [1, 9] = 0
  | otherwise = 1

composition :: Rank -> Int
composition rank
  | elem rank [1, 7] = 2
  | otherwise = 1

ranks = [0 .. 9]

allSuits = [Red .. Green]

reducedSuits = [Red .. Blue]

deck suits =
  [ Card {rank = rank, suit = suit}
  | rank <- ranks
  , suit <- suits
  , _ <- [1 .. (composition rank)]
  ]

reducedDeck = deck reducedSuits

fullDeck = deck allSuits

newtype ChiefTrump =
  ChiefTrump Trump

--data Trumps = Trumps { chiefTrump :: Trump, viceTrump :: Maybe Trump }
--  deriving (Show)
data Trump
  = SuitTrump Suit
  | RankTrump Int
  | NoTrump
  deriving (Eq, Show)
