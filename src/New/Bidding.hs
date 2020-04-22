--{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module New.Bidding where

import New.GamePlay
import Cards
import AuctionFunctions(Bid(..), Winners(..))
import Control.Monad.State.Class
import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.List

data FinishedBidding f player
  = Successful (SuccessfulBidding player)
  | Unsuccessful (Stalemate f player)

data SuccessfulBidding player = SuccessfulBidding 
  { biddingWinners :: Winners player
  }

data Stalemate f player
  = EklatNoPoints
  | Eklat { atFault  :: player
          , affected :: f player
          , topBid   :: Int }
  deriving (Eq, Show)

initialPositions :: [Card] -> CardPositions 
initialPositions cards = CardPositions{inHand = cards, onTable = []}

type GetBid f player = player -> [Card] -> f Bid

runAuction
  :: (Monad m, Applicative players) =>
     (player -> m Bid)
     -> (player -> ASetter' (players [Card]) [Card])
     -> m player
     -> Int
     -> m (players [Card])
runAuction getBid setter players numberOfPlayers = runBidding' numberOfPlayers where
  runBidding' 0 = return $ pure []
  runBidding' passesLeft = do
    player <- players
    bid <- getBid player
    case bid of 
      Pass -> runBidding' (passesLeft - 1)
      Raise cards -> over (setter player) (cards ++) <$> runBidding' numberOfPlayers

tallyAuction fold traversal cards cards2 players playerCards = case maxBidders playerCards of
  [] -> Unsuccessful (EklatNoPoints)
  [player] -> Successful (SuccessfulBidding (winners player playerCards))
  lastPlayerToRaise:others -> Unsuccessful (Eklat
    { atFault = lastPlayerToRaise 
    , affected = others
    , topBid = topBid
    })
  where
    maxBidders = undefined
    winners = undefined
    playerBidSizes = over (traversal ) length playerCards
    maxBid = maximum1Of (fold ) playerBidSizes
    topBid = undefined

--try pairs = maximum1Of (threeFold . _2) counts
--  where
--    counts = over (threeFold . _2) length pairs
    
--tallyAuction fold = undefined
--  where
winners f1 f2 f3 max playerCards = toListOf (f1 . filtered ((==max) . view f2) . f3)

maxBid2 fold cards traversal playerCards = maximum1Of (fold ) (over (traversal . cards) length playerCards)

--maxBid playerCards = maximum1Of threeFold (over threeFold length playerCards)

data Three a = Three a a a

threeFold :: (Apply f) => (a -> f b) -> Three a -> f (Three b) 
--threeFold :: Fold1 (Three a) a 
threeFold f (Three a b c) = Three <$> f a <.> f b <.> f c

playerCards :: Three [Int]
playerCards = Three [1,2] [1] [3]

playerBidSizes f = over (f) (length ) playerCards

maxKeys three = toListOf (threeFold . filtered hasMax . _1) three
  where 
    hasMax (_, b) = b == max
    max = maximum1Of (threeFold . _2) three

maxKeys2 f g three = toListOf (f . filtered hasMax . _1) three
  where 
    hasMax (_, b) = b == max
    max = maximum1Of (g . _2) three

--maxBid :: (forall a b. (Apply f) => (a -> f b) -> Three a -> f (Three b)) -> Int
--maxBid g = maximum1Of g (playerBidSizes g)

--maxBid2 f = maximum1Of (f . _2) (over (f . _2) (length ) playerCards :: Three (Int, Int))
--maxBidders  = toListOf (threeFold . filtered (\t -> snd t == maxBid) . _1)  playerBidSizes 

mkpair1 :: a -> b -> (a,b)
mkpair1 aa bb = (ida aa, bb)
    where
      ida :: a -> a -- This refers to a in the function's type signature
      ida = id

sortByIndex :: (Eq a) => [a] -> [a] -> [a]
sortByIndex order = sortOn $ flip elemIndex order

data Typ = Myint Integer | Myotherint Integer

func (Myint _) = True
func (Myotherint _) = True

