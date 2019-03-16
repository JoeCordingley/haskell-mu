{-# LANGUAGE GADTs #-}

module AuctionState
  (
  ) where

import           Cards

data Player n where
  PlayerOneOfThree :: Player ThreePlayers
  PlayerTwoOfThree :: Player ThreePlayers
  PlayerThreeOfThree :: Player ThreePlayers
  PlayerOneOfFour :: Player FourPlayers
  PlayerTwoOfFour :: Player FourPlayers
  PlayerThreeOfFour :: Player FourPlayers
  PlayerFourOfFour :: Player FourPlayers
  PlayerOneOfFive :: Player FivePlayers
  PlayerTwoOfFive :: Player FivePlayers
  PlayerThreeOfFive :: Player FivePlayers
  PlayerFourOfFive :: Player FivePlayers
  PlayerFiveOfFive :: Player FivePlayers
  PlayerOneOfSix :: Player SixPlayers
  PlayerTwoOfSix :: Player SixPlayers
  PlayerThreeOfSix :: Player SixPlayers
  PlayerFourOfSix :: Player SixPlayers
  PlayerFiveOfSix :: Player SixPlayers
  PlayerSixOfSix :: Player SixPlayers

data Bid
  = Pass
  | Raise [Card]

data AuctionState n = AuctionState
  { cardsInHand :: PlayerCards n
  , passes      :: Int
  , cardsBid    :: PlayerCards n
  , lastToRaise :: [Player n]
  }

data ThreePlayers 
data FourPlayers 
data FivePlayers 
data SixPlayers

data PlayerCards n where
  ThreePlayerCards
    :: { playerOneOfThree :: [Card]
       , playerTwoOfThree :: [Card]
       , playerThreeOfThree :: [Card]}
    -> PlayerCards ThreePlayers
  FourPlayerCards
    :: { playerOneOfFour :: [Card]
       , playerTwoOfFour :: [Card]
       , playerThreeOfFour :: [Card]
       , playerFourOfFour :: [Card]} -> PlayerCards FourPlayers
  FivePlayerCards 
    :: { playerOneOfFive :: [Card]
       , playerTwoOfFive :: [Card]
       , playerThreeOfFive :: [Card]
       , playerFourOfFive :: [Card]
       , playerFiveOfFive :: [Card]} -> PlayerCards FivePlayers
  SixPlayerCards
    :: { playerOneOfSix :: [Card]
       , playerTwoOfSix :: [Card]
       , playerThreeOfSix :: [Card]
       , playerFourOfSix :: [Card]
       , playerFiveOfSix :: [Card]
       , playerSixOfSix :: [Card]} -> PlayerCards SixPlayers

playersCards :: Player numberOfPlayers -> PlayerCards numberOfPlayers -> [Card]
playersCards PlayerOneOfThree = playerOneOfThree
playersCards PlayerTwoOfThree = playerTwoOfThree
playersCards PlayerThreeOfThree = playerThreeOfThree
playersCards PlayerOneOfFour = playerOneOfFour 
playersCards PlayerTwoOfFour = playerTwoOfFour
playersCards PlayerThreeOfFour = playerThreeOfFour
playersCards PlayerFourOfFour = playerFourOfFour
playersCards PlayerOneOfFive = playerOneOfFive
playersCards PlayerTwoOfFive = playerTwoOfFive
playersCards PlayerThreeOfFive = playerThreeOfFive
playersCards PlayerFourOfFive = playerFourOfFive
playersCards PlayerFiveOfFive = playerFiveOfFive
playersCards PlayerOneOfSix = playerOneOfSix
playersCards PlayerTwoOfSix = playerTwoOfSix
playersCards PlayerThreeOfSix = playerThreeOfSix
playersCards PlayerFourOfSix = playerFourOfSix
playersCards PlayerFiveOfSix = playerFiveOfSix
playersCards PlayerSixOfSix = playerSixOfSix 

auctionState ::
     Player n -> Bid -> AuctionState n -> AuctionState n
auctionState player Pass state = state {passes = passes state + 1}
--auctionState player (Raise cards) state = state {cardsInHand = cardsReduced, cardsBid = cardsIncreased, lastToRaise = player: (lastToRaise state)} where
--  cardsReduced = undefined
--  playersHand = undefined
--  cardsIncreased = undefined

--data ThreePlayers
--  = PlayerOneOfThree
--  | PlayerTwoOfThree
--  | PlayerThreeOfThree
--
--data FourPlayers
--  = PlayerOneOfFour
--  | PlayerTwoOfFour
--  | PlayerThreeOfFour
--  | PlayerFourOfFour
--
--data FivePlayers
--  = PlayerOneOfFive
--  | PlayerTwoOfFive
--  | PlayerThreeOfFive
--  | PlayerFourOfFive
--  | PlayerFiveOfFive
--
--data SixPlayers
--  = PlayerOneOfSix
--  | PlayerTwoOfSix
--  | PlayerThreeOfSix
--  | PlayerFourOfSix
--  | PlayerFiveOfSix
--  | PlayerSixOfSix
--
