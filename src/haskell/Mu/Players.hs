{-# LANGUAGE RankNTypes #-}

module Mu.Players where

import           Control.Lens
import           Control.Monad.State.Lazy
import           Data.Aeson
import           Data.Foldable
import           Data.Tuple.Homogenous

class Cycling a where
  successor :: a -> a

except players player tuple = filtered =<< toList ((,) <$> players <*> tuple)
  where
    filtered (player', a) =
      if player' == player
        then []
        else [a]

getPlayer :: (player -> Getting a players a) -> player -> players -> a
getPlayer lens player tuple = view (lens player) tuple

data NOfThree
  = OneOfThree
  | TwoOfThree
  | ThreeOfThree
  deriving (Eq, Enum, Show)

exceptThree :: NOfThree -> Tuple3 a -> [a]
exceptThree = except threePlayers

getThree :: NOfThree -> Tuple3 a -> a
getThree = getPlayer threeLens

instance ToJSON a => ToJSON (Tuple3 a) where
  toJSON (Tuple3 t) = toJSON t

instance ToJSON NOfThree where
  toJSON OneOfThree   = Number 1
  toJSON TwoOfThree   = Number 2
  toJSON ThreeOfThree = Number 3

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

exceptFour :: NOfFour -> Tuple4 a -> [a]
exceptFour = except fourPlayers

getFour :: NOfFour -> Tuple4 a -> a
getFour = getPlayer fourLens

instance ToJSON a => ToJSON (Tuple4 a) where
  toJSON (Tuple4 t) = toJSON t

instance ToJSON NOfFour where
  toJSON OneOfFour   = Number 1
  toJSON TwoOfFour   = Number 2
  toJSON ThreeOfFour = Number 3
  toJSON FourOfFour  = Number 4

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

exceptFive :: NOfFive -> Tuple5 a -> [a]
exceptFive = except fivePlayers

getFive :: NOfFive -> Tuple5 a -> a
getFive = getPlayer fiveLens

instance ToJSON a => ToJSON (Tuple5 a) where
  toJSON (Tuple5 t) = toJSON t

instance ToJSON NOfFive where
  toJSON OneOfFive   = Number 1
  toJSON TwoOfFive   = Number 2
  toJSON ThreeOfFive = Number 3
  toJSON FourOfFive  = Number 4
  toJSON FiveOfFive  = Number 5

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

exceptSix :: NOfSix -> Tuple6 a -> [a]
exceptSix = except sixPlayers

getSix :: NOfSix -> Tuple6 a -> a
getSix = getPlayer sixLens

instance ToJSON a => ToJSON (Tuple6 a) where
  toJSON (Tuple6 t) = toJSON t

instance ToJSON NOfSix where
  toJSON OneOfSix   = Number 1
  toJSON TwoOfSix   = Number 2
  toJSON ThreeOfSix = Number 3
  toJSON FourOfSix  = Number 4
  toJSON FiveOfSix  = Number 5
  toJSON SixOfSix   = Number 6

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
