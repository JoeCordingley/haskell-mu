{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TupleSections    #-}

module Mu.CardPlay where

import           Cards                    (Card (..), ChiefTrump (..), Suit,
                                           Trump (..))
import           Control.Applicative
import           Control.Lens             hiding ((<.>))
import           Control.Monad.Reader
import           Control.Monad.State.Lazy
import           Data.Foldable            (fold)
import           Data.Function.Syntax
import           Data.Functor.Apply
import           Data.Functor.Bind
import           Data.Functor.Compose
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Foldable
import           Mu.Auction               (ViceTrump (..))
import           Mu.Auction               (CardPositions (..), Chief (..))
import           Mu.Players               (Cycling, players)
import           Util                     (remove)

newtype Ap f a =
  Ap
    { getAp :: f a
    }

newtype NumberOfRounds =
  NumberOfRounds Int

instance (Apply m, Semigroup a) => Semigroup (Ap m a) where
  Ap l <> Ap r = Ap $ fmap (<>) l <.> r

playTrick ::
     (Apply m, Foldable1 players)
  => (player -> m Card)
  -> ChiefTrump
  -> Maybe ViceTrump
  -> players player
  -> m (WonTrick player)
playTrick getCard chiefTrump viceTrump =
  fmap wonTrick . getAp . foldMap1 (Ap . getCard')
  where
    getCard' player = fmap (winnerAndCards player) $ getCard player
    winnerAndCards player card =
      ( [card]
      , ( First $ suit card
        , \ledSuit ->
            TrickWinner player (trickCard chiefTrump viceTrump ledSuit card)))
    wonTrick (cards, (First suit, trickWinner)) =
      WonTrick (winningPlayer . trickWinner $ LedSuit suit) cards

data WonTrick player =
  WonTrick
    { winner :: player
    , trick  :: [Card]
    }

playTrickStateful ::
     (Monad m, Bind m, Foldable1 players)
  => (player -> LensLike' (Compose m ((,) (Card, Maybe FirstCard))) (players cards) [PlayableCard])
  -> (player -> [PlayableCard] -> m PlayableCard)
  -> ChiefTrump
  -> Maybe ViceTrump
  -> players player
  -> players cards
  -> m (WonTrick player, players cards)
playTrickStateful l getCard chiefTrump viceTrump players =
  runStateT (evalStateT playTrick' Nothing)
  where
    playTrick' =
      playTrick
        (\player -> StateT (StateT . getCard' player))
        chiefTrump
        viceTrump
        players
    getCard' player firstCard =
      getCompose . (l player) (Compose . getAndRemove player firstCard)
    getAndRemove player firstCard cards =
      fmap (remove' firstCard cards) . getCard player $
      allowedCards' firstCard cards
    remove' firstCard playableCards (card, cardPosition) =
      ( (card, firstCard <|> Just (FirstCard card))
      , remove (card, cardPosition) playableCards)
    allowedCards' Nothing cards = cards
    allowedCards' (Just firstCard) cards =
      allowedCards chiefTrump viceTrump firstCard cards

playCardsStateful ::
     ( Monoid (players [Card])
     , Monoid (players ())
     , Cycling player
     , Monad m
     , Bind m
     , Traversable1 players
     , Functor players
     )
  => (forall c. player -> Lens' (players c) c)
  -> (player -> [PlayableCard] -> m PlayableCard)
  -> NumberOfRounds
  -> ChiefTrump
  -> Maybe ViceTrump
  -> Chief player
  -> players CardPositions
  -> m (players [Card])
playCardsStateful = playCardsStatefulWithUpdate doNothing
  where
    doNothing _ = pure ()

playCardsStatefulWithUpdate ::
     ( Monoid (players [Card])
     , Monoid (players ())
     , Cycling player
     , Monad m
     , Bind m
     , Traversable1 players
     , Functor players
     )
  => (WinnerOfTrick player -> m ())
  -> (forall c. player -> Lens' (players c) c)
  -> (player -> [PlayableCard] -> m PlayableCard)
  -> NumberOfRounds
  -> ChiefTrump
  -> Maybe ViceTrump
  -> Chief player
  -> players CardPositions
  -> m (players [Card])
playCardsStatefulWithUpdate update l getCard (NumberOfRounds numberOfRounds) chiefTrump viceTrump (Chief chief) cardPositions =
  evalStateT roundState (chief, playableCards')
  where
    playableCards' = fmap playableCards cardPositions
    roundState = fold <$> replicateM numberOfRounds (StateT trickState)
    trickState (player, cards) = do
      (WonTrick {winner, trick}, playerCards) <-
        playTrickStateful
          l
          getCard
          chiefTrump
          viceTrump
          (playersStartingFrom player)
          cards
      update (WinnerOfTrick winner)
      pure (set (l winner) trick mempty, (winner, playerCards))

newtype WinnerOfTrick player =
  WinnerOfTrick
    { getWinner :: player
    }

playableCards :: CardPositions -> [PlayableCard]
playableCards c = (fmap (, InHand) (inHand c)) <> (fmap (, OnTable) (onTable c))

playersStartingFrom ::
     (Traversable f, Monoid (f ()), Cycling player) => player -> f player
playersStartingFrom = evalState $ traverseEmpty players

traverseEmpty :: (Monoid (t ()), Traversable t, Applicative f) => f b -> f (t b)
traverseEmpty fb = traverse f mempty
  where
    f () = fb

allowedCards ::
     ChiefTrump
  -> Maybe ViceTrump
  -> FirstCard
  -> [PlayableCard]
  -> [PlayableCard]
allowedCards chiefTrump viceTrump (FirstCard firstCard) cards =
  case following of
    []       -> cards
    nonEmpty -> nonEmpty
  where
    following =
      if isATrumpCard' firstCard
        then trumpCards
        else suitCards
    trumpCards = filter (isATrumpCard' . fst) cards
    suitCards = filter (ofLedSuit . fst) cards
    ofLedSuit card = suit card == suit firstCard && not (isATrumpCard' card)
    isATrumpCard' = isATrumpCard chiefTrump viceTrump

newtype FirstCard =
  FirstCard Card

data TrickWinner player =
  TrickWinner
    { winningPlayer :: player
    , winningCard   :: TrickCard
    }

data TrickCard
  = OtherCard
  | CardOfLedSuit Rank
  | TrumpCard TrumpCard
  deriving (Eq, Ord)

newtype Rank =
  Rank Int
  deriving (Eq, Ord)

data TrumpCard
  = LowerTrump Rank
  | HighestTrump Rank
  | DoublyTrump
  deriving (Eq, Ord)

instance Semigroup (TrickWinner player) where
  TrickWinner earlierPlayer earlierCard <> TrickWinner laterPlayer laterCard =
    if earlierCard >= laterCard
      then TrickWinner earlierPlayer earlierCard
      else TrickWinner laterPlayer laterCard

trickCard :: ChiefTrump -> Maybe ViceTrump -> LedSuit -> Card -> TrickCard
trickCard (ChiefTrump chiefTrump) viceTrump (LedSuit ledSuit) card
  | isHighestTrump && isLowerTrump = TrumpCard DoublyTrump
  | isHighestTrump = TrumpCard (HighestTrump rank')
  | isLowerTrump = TrumpCard (LowerTrump rank')
  | isOfLedSuit = CardOfLedSuit rank'
  | otherwise = OtherCard
  where
    isHighestTrump = isTrump chiefTrump card
    isLowerTrump = any (\(ViceTrump trump) -> isTrump trump card) viceTrump
    isOfLedSuit = suit card == ledSuit
    rank' = Rank (rank card)

newtype LedSuit =
  LedSuit Suit

isTrump :: Trump -> Card -> Bool
isTrump (SuitTrump trumpSuit) (Card cardSuit _) = trumpSuit == cardSuit
isTrump (RankTrump trumpRank) (Card _ cardRank) = trumpRank == cardRank
isTrump NoTrump _                               = False

isATrumpCard :: ChiefTrump -> Maybe ViceTrump -> Card -> Bool
isATrumpCard (ChiefTrump chiefTrump) viceTrump card =
  isTrump chiefTrump card || any (isViceTrump card) viceTrump
  where
    isViceTrump card (ViceTrump trump) = isTrump trump card

type PlayableCard = (Card, CardPosition)

data CardPosition
  = InHand
  | OnTable
  deriving (Eq)
