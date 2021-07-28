module Cards where

import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import CSS
import Prelude

type Rank = Int

data Suit
  = Red
  | Black
  | Blue
  | Yellow
  | Green

instance showSuit :: Show Suit where
  show = case _ of
    Red -> "Red"
    Black -> "Black"
    Blue -> "Blue"
    Yellow -> "Yellow"
    Green -> "Green"

data Card =
  Card
    { suit :: Suit
    , rank :: Rank
    }

suitColor :: Suit -> Color
suitColor = case _ of
  Red -> red
  Black -> black
  Blue -> blue
  Yellow -> yellow
  Green -> green

instance showCard :: Show Card where
  show (Card {suit, rank}) = show suit <> " " <> show rank

render :: forall w i. Card -> HH.HTML w i
render card@(Card {suit} ) = HH.div [ CSS.style do backgroundColor (suitColor suit) ] [ HH.text (show card) ]
