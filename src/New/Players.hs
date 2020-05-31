{-# LANGUAGE RankNTypes #-}

module New.Players where

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Tuple.Homogenous

class Cycling a where
  successor :: a -> a

data NOfThree
  = OneOfThree
  | TwoOfThree
  | ThreeOfThree
  deriving (Eq, Enum, Show)

threeLens :: NOfThree -> Lens' (Tuple3 a) a
threeLens n = threeIso . l
  where
    l =
      case n of
        OneOfThree   -> _1
        TwoOfThree   -> _2
        ThreeOfThree -> _3

threeIso :: Iso' (Tuple3 a) (a, a, a)
threeIso = iso untuple3 Tuple3

data NOfFour
  = OneOfFour
  | TwoOfFour
  | ThreeOfFour
  | FourOfFour
  deriving (Eq, Enum, Show)

fourLens :: NOfFour -> Lens' (Tuple4 a) a
fourLens n = fourIso . l
  where
    l =
      case n of
        OneOfFour   -> _1
        TwoOfFour   -> _2
        ThreeOfFour -> _3
        FourOfFour  -> _4

fourIso :: Iso' (Tuple4 a) (a, a, a, a)
fourIso = iso untuple4 Tuple4

data NOfFive
  = OneOfFive
  | TwoOfFive
  | ThreeOfFive
  | FourOfFive
  | FiveOfFive
  deriving (Eq, Enum, Show)

fiveLens :: NOfFive -> Lens' (Tuple5 a) a
fiveLens n = fiveIso . l
  where
    l =
      case n of
        OneOfFive   -> _1
        TwoOfFive   -> _2
        ThreeOfFive -> _3
        FourOfFive  -> _4
        FiveOfFive  -> _5

fiveIso :: Iso' (Tuple5 a) (a, a, a, a, a)
fiveIso = iso untuple5 Tuple5

data NOfSix
  = OneOfSix
  | TwoOfSix
  | ThreeOfSix
  | FourOfSix
  | FiveOfSix
  | SixOfSix
  deriving (Eq, Enum, Show)

sixLens :: NOfSix -> Lens' (Tuple6 a) a
sixLens n = sixIso . l
  where
    l =
      case n of
        OneOfSix   -> _1
        TwoOfSix   -> _2
        ThreeOfSix -> _3
        FourOfSix  -> _4
        FiveOfSix  -> _5
        SixOfSix   -> _6

sixIso :: Iso' (Tuple6 a) (a, a, a, a, a, a)
sixIso = iso untuple6 Tuple6

instance Cycling NOfThree where
  successor ThreeOfThree = OneOfThree
  successor other        = succ other

instance Cycling NOfFour where
  successor FourOfFour = OneOfFour
  successor other      = succ other

instance Cycling NOfFive where
  successor FiveOfFive = OneOfFive
  successor other      = succ other

instance Cycling NOfSix where
  successor SixOfSix = OneOfSix
  successor other    = succ other

threePlayers = Tuple3 (OneOfThree, TwoOfThree, ThreeOfThree)

fourPlayers = Tuple4 (OneOfFour, TwoOfFour, ThreeOfFour, FourOfFour)

fivePlayers = Tuple5 (OneOfFive, TwoOfFive, ThreeOfFive, FourOfFive, FiveOfFive)

sixPlayers =
  Tuple6 (OneOfSix, TwoOfSix, ThreeOfSix, FourOfSix, FiveOfSix, SixOfSix)

players :: (Cycling player, MonadState player m) => m player
players = state nextPlayer
  where
    nextPlayer player = (player, successor player)
