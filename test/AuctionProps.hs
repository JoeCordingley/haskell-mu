{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module AuctionProps(auctionProperties) where

import Test.Tasty
import Test.Tasty.QuickCheck hiding (shuffle)
import qualified Test.Tasty.QuickCheck as QC (shuffle)
import Cards
import Control.Monad.Writer.Lazy
import Deal hiding (shuffle)
import qualified Deal as Deal (shuffle)
import AuctionPlay2
import Data.List.NonEmpty (NonEmpty(..))

auctionProperties :: TestTree
auctionProperties = testGroup "Auction Properties" [ biddingProperties ]

biddingProperties :: TestTree
biddingProperties = testGroup "Bidding Properties" [playersAskedInTurn]

type Player = Int

bidGen :: Int -> player -> [Card] -> Gen Bid
bidGen maxBid player cardsToChooseFrom = do
  numberOfCards <- choose (0, maxBid)
  bidFromCards <$> sublistOfN numberOfCards cardsToChooseFrom

bidGenPlayerRecorded :: Int -> Player -> [Card] -> WriterT [Player] Gen Bid
bidGenPlayerRecorded maxBid player cardsToChooseFrom = do 
  tell [player] 
  lift $ bidGen maxBid player cardsToChooseFrom

instance Shuffle Gen where
  shuffle as = QC.shuffle as

initialHandsGen :: Gen [(Player, [Card])]
initialHandsGen = do
  numberOfPlayers <- choose (3, 6)
  let deck = if numberOfPlayers == 3 then reducedDeck else fullDeck 
      players = [1..numberOfPlayers]
  deal players deck

instance GetBid Gen p where
  getBid = bidGen

data BidRecord p = BidRecord 
  { maxBidRecorded :: Int
  , playerRecorded :: p
  , cardsRecorded :: [Card]
  , bidRecorded :: Bid
  }

instance GetBid m p => GetBid (WriterT [BidRecord p] m) p  where
  getBid max player cards = do
    bid <- lift $ getBid max player cards
    tell [BidRecord 
      { maxBidRecorded = max
      , playerRecorded = player
      , cardsRecorded = cards
      , bidRecorded = bid
      }]
    return bid

genPlayersAsked :: Gen [BidRecord Player] 
genPlayersAsked = do
  initialHands <- initialHandsGen
  execWriterT $ bidding initialHands

bidFromCards :: [Card] -> Bid
bidFromCards [] = Pass
bidFromCards (c: cs) = Raise (c:|cs)

sublistOfN :: Eq a => Int -> [a] -> Gen [a]
sublistOfN 0 _ = return []
sublistOfN _ [] = return []
sublistOfN n as = do 
  a <- elements as
  let remaining = remove a as
  rest <- sublistOfN (n -1) remaining
  return $ a : rest

remove :: Eq a => a  -> [a] -> [a]
remove a []= []
remove a (a':as) 
  | a == a' = as
  | otherwise = remove a as

playersAskedInTurn :: TestTree
playersAskedInTurn  = testProperty "Players are asked in turn" $ forAll bidGen playersAskedInTurn' where
  playersAskedInTurn' = undefined :: () -> Bool
  bidGen = undefined :: Gen ()

