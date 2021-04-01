module Cards where

type Rank = Int

data Suit
  = Red
  | Black
  | Blue
  | Yellow
  | Green

data Card =
  Card
    { suit :: Suit
    , rank :: Rank
    }
 
