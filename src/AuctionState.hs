{-# LANGUAGE GADTs, RankNTypes, TemplateHaskell, StandaloneDeriving,  MultiParamTypeClasses #-}


module AuctionState
  ( Player(..)
  , statePasses
  , NumberOf(..)
  , AuctionState(..)
  ) where

import           Cards
import Control.Lens
import Control.Applicative

--data Player n where
--  PlayerOneOfThree :: Player ThreePlayers
--  PlayerTwoOfThree :: Player ThreePlayers
--  PlayerThreeOfThree :: Player ThreePlayers
--  PlayerOneOfFour :: Player FourPlayers
--  PlayerTwoOfFour :: Player FourPlayers
--  PlayerThreeOfFour :: Player FourPlayers
--  PlayerFourOfFour :: Player FourPlayers
--  PlayerOneOfFive :: Player FivePlayers
--  PlayerTwoOfFive :: Player FivePlayers
--  PlayerThreeOfFive :: Player FivePlayers
--  PlayerFourOfFive :: Player FivePlayers
--  PlayerFiveOfFive :: Player FivePlayers
--  PlayerOneOfSix :: Player SixPlayers
--  PlayerTwoOfSix :: Player SixPlayers
--  PlayerThreeOfSix :: Player SixPlayers
--  PlayerFourOfSix :: Player SixPlayers
--  PlayerFiveOfSix :: Player SixPlayers
--  PlayerSixOfSix :: Player SixPlayers
data Bid
  = Pass
  | Raise [Card]

data AuctionState n = AuctionState
  { cardsInHand :: PlayerCards n
  , passes      :: Int
  , cardsBid    :: PlayerCards n
  , lastToRaise :: [Player n]
  }

data Player n where
  PlayerOfThree :: ThreePlayers -> Player ThreePlayers 
  PlayerOfFour :: FourPlayers -> Player FourPlayers 
  PlayerOfFive :: FivePlayers -> Player FivePlayers 
  PlayerOfSix :: SixPlayers -> Player SixPlayers 

data PlayerA n where
  PlayerB :: PlayerA Int 

deriving instance Eq n => Eq (Player n)
deriving instance Show n => Show (Player n)

class NumberOf n where
  numberOf :: f n -> Int

instance NumberOf ThreePlayers where
  numberOf _ = 3
instance NumberOf FourPlayers where
  numberOf _ = 4
instance NumberOf FivePlayers where
  numberOf _ = 5
instance NumberOf SixPlayers where
  numberOf _ = 6

data ThreePlayers
  = PlayerOneOfThree
  | PlayerTwoOfThree
  | PlayerThreeOfThree

data FourPlayers
  = PlayerOneOfFour
  | PlayerTwoOfFour
  | PlayerThreeOfFour
  | PlayerFourOfFour

data FivePlayers
  = PlayerOneOfFive
  | PlayerTwoOfFive
  | PlayerThreeOfFive
  | PlayerFourOfFive
  | PlayerFiveOfFive

data SixPlayers
  = PlayerOneOfSix
  | PlayerTwoOfSix
  | PlayerThreeOfSix
  | PlayerFourOfSix
  | PlayerFiveOfSix
  | PlayerSixOfSix

type PlayerCards n = PlayerMap n [Card]
data PlayerMap n c where
  ThreePlayerMap
    :: { playerOneOfThree :: c
       , playerTwoOfThree :: c
       , playerThreeOfThree :: c}
    -> PlayerMap ThreePlayers c
  FourPlayerMap
    :: { playerOneOfFour :: c 
       , playerTwoOfFour :: c
       , playerThreeOfFour :: c
       , playerFourOfFour :: c}
    -> PlayerMap FourPlayers c
  FivePlayerMap
    :: { playerOneOfFive :: c
       , playerTwoOfFive :: c
       , playerThreeOfFive :: c
       , playerFourOfFive :: c
       , playerFiveOfFive :: c}
    -> PlayerMap FivePlayers c
  SixPlayerMap
    :: { playerOneOfSix :: c
       , playerTwoOfSix :: c
       , playerThreeOfSix :: c
       , playerFourOfSix :: c
       , playerFiveOfSix :: c
       , playerSixOfSix :: c}
    -> PlayerMap SixPlayers c

instance Functor (PlayerMap n) where
  fmap f t = runIdentity $ traverse (Identity . f) t

instance Foldable (PlayerMap n) where
  foldMap f (ThreePlayerMap c1 c2 c3) = f c1 <> f c2 <> f c3
  foldMap f (FourPlayerMap c1 c2 c3 c4) = f c1 <> f c2 <> f c3 <> f c4
  foldMap f (FivePlayerMap c1 c2 c3 c4 c5) = f c1 <> f c2 <> f c3 <> f c4 <> f c5
  foldMap f (SixPlayerMap c1 c2 c3 c4 c5 c6) = f c1 <> f c2 <> f c3 <> f c4 <> f c5 <> f c6

instance Traversable (PlayerMap n) where
  traverse f (ThreePlayerMap c1 c2 c3) = pure ThreePlayerMap <*> f c1 <*> f c2 <*> f c3
  traverse f (FourPlayerMap c1 c2 c3 c4) = pure FourPlayerMap <*> f c1 <*> f c2 <*> f c3 <*> f c4
  traverse f (FivePlayerMap c1 c2 c3 c4 c5) = pure FivePlayerMap <*> f c1 <*> f c2 <*> f c3 <*> f c4 <*> f c5
  traverse f (SixPlayerMap c1 c2 c3 c4 c5 c6) = pure SixPlayerMap <*> f c1 <*> f c2 <*> f c3 <*> f c4 <*> f c5 <*> f c6



playerMap :: Player n -> Lens' (PlayerMap n c) c
playerMap (PlayerOfThree PlayerOneOfThree) f s = fmap (\a -> s{playerOneOfThree=a}) (f $ playerOneOfThree s)
playerMap (PlayerOfThree PlayerTwoOfThree) f s = fmap (\a -> s{playerTwoOfThree=a}) (f $ playerTwoOfThree s)
playerMap (PlayerOfThree PlayerThreeOfThree) f s = fmap (\a -> s{playerThreeOfThree=a}) (f $ playerThreeOfThree s)
playerMap (PlayerOfFour PlayerOneOfFour) f s = fmap (\a -> s{playerOneOfFour=a}) (f $ playerOneOfFour s)
playerMap (PlayerOfFour PlayerTwoOfFour) f s = fmap (\a -> s{playerTwoOfFour=a}) (f $ playerTwoOfFour s)
playerMap (PlayerOfFour PlayerThreeOfFour) f s = fmap (\a -> s{playerThreeOfFour=a}) (f $ playerThreeOfFour s)
playerMap (PlayerOfFour PlayerFourOfFour) f s = fmap (\a -> s{playerFourOfFour=a}) (f $ playerFourOfFour s)
playerMap (PlayerOfFive PlayerOneOfFive) f s = fmap (\a -> s{playerOneOfFive=a}) (f $ playerOneOfFive s)
playerMap (PlayerOfFive PlayerTwoOfFive) f s = fmap (\a -> s{playerTwoOfFive=a}) (f $ playerTwoOfFive s)
playerMap (PlayerOfFive PlayerThreeOfFive) f s = fmap (\a -> s{playerThreeOfFive=a}) (f $ playerThreeOfFive s)
playerMap (PlayerOfFive PlayerFourOfFive) f s = fmap (\a -> s{playerFourOfFive=a}) (f $ playerFourOfFive s)
playerMap (PlayerOfFive PlayerFiveOfFive) f s = fmap (\a -> s{playerFiveOfFive=a}) (f $ playerFiveOfFive s)
playerMap (PlayerOfSix PlayerOneOfSix) f s = fmap (\a -> s{playerOneOfSix=a}) (f $ playerOneOfSix s)
playerMap (PlayerOfSix PlayerTwoOfSix) f s = fmap (\a -> s{playerTwoOfSix=a}) (f $ playerTwoOfSix s)
playerMap (PlayerOfSix PlayerThreeOfSix) f s = fmap (\a -> s{playerThreeOfSix=a}) (f $ playerThreeOfSix s)
playerMap (PlayerOfSix PlayerFourOfSix) f s = fmap (\a -> s{playerFourOfSix=a}) (f $ playerFourOfSix s)
playerMap (PlayerOfSix PlayerFiveOfSix) f s = fmap (\a -> s{playerFiveOfSix=a}) (f $ playerFiveOfSix s)
playerMap (PlayerOfSix PlayerSixOfSix) f s = fmap (\a -> s{playerSixOfSix=a}) (f $ playerSixOfSix s)

playerTraversal :: Traversal' (PlayerMap n c) c
playerTraversal = traverse 

stateCardsBid :: Lens' (AuctionState n) (PlayerCards n)
stateCardsBid f s = fmap (\a -> s{cardsBid=a}) (f $ cardsBid s)

stateCardsInHand :: Lens' (AuctionState n) (PlayerCards n)
stateCardsInHand f s = fmap (\a -> s{cardsInHand=a}) (f $ cardsInHand s)

statePasses :: Lens' (AuctionState n) Int
statePasses f s = fmap (\a -> s{passes=a}) (f $ passes s)

stateRaises :: Lens' (AuctionState n) [Player n]
stateRaises f s = fmap (\a -> s {lastToRaise=a}) (f $ lastToRaise s)

playerBids :: Player n -> Lens' (AuctionState n) [Card]
playerBids player = stateCardsBid . playerMap player

playerHand :: Player n -> Lens' (AuctionState n) [Card]
playerHand player = stateCardsBid . playerMap player

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys)
  | x == y = ys
  | otherwise = y : (remove x ys)

minus :: Eq a => [a] -> [a] -> [a]
minus xs ys = foldr remove xs ys

auctionState :: Player n -> Bid -> AuctionState n -> AuctionState n
auctionState player Pass = over statePasses (+1)
auctionState player (Raise cards) = over (playerBids player) (++cards) . over (playerHand player) (minus cards) . over stateRaises (player:)

