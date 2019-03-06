module Cards
  ( someFunc
  , Card(..)
  , Suit(..)
  , Rank
  , points
  , fullDeck
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Rank = Int

data Suit
  = Red
  | Black
  | Blue
  | Yellow
  | Green
  deriving (Enum, Eq, Show)

data Card = Card
  { suit :: Suit
  , rank :: Rank
  } deriving (Eq, Show)

points :: Rank -> Int
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
