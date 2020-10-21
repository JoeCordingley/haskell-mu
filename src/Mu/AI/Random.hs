module Mu.AI.Random where

import           Cards                (Card, Trump)
import           Control.Monad.Random
import           Mu.Auction           (Bid, ChiefTrump (..), MaxRaise (..),
                                       Partner (..), ViceTrump (..), toBid)
import           Mu.CardPlay          (PlayableCard)
import           Mu.GamePlay          (MoveUpdates (..),
                                       SingularDependencies (..),
                                       SingularUpdates (..))

expMult = -1

exponential i =
  weighted $
  fmap (\i -> (i, realToFrac $ exp (expMult * (fromIntegral i)))) [0 .. i]

choose n as = choose' (fromIntegral (length as)) n as
  where
    choose' 0 _ _ = pure []
    choose' n c (a:as)
      | n == c = pure (a : as)
      | otherwise = do
        take <- weighted [(True, c), (False, n - c)]
        if take
          then (a :) <$> choose' (n - 1) (c - 1) as
          else choose' (n - 1) c as

requestBid :: MonadRandom m => MaxRaise -> [Card] -> m Bid
requestBid (MaxRaise maxRaise) cards = do
  n <- fromIntegral <$> exponential (min maxRaise (length cards))
  toBid <$> choose n cards

requestViceTrump :: (MonadRandom m, Foldable f) => f Trump -> m ViceTrump
requestViceTrump = fmap ViceTrump . uniform

requestChiefTrump :: (MonadRandom m, Foldable f) => f Trump -> m ChiefTrump
requestChiefTrump = fmap ChiefTrump . uniform

requestPartner ::
     (MonadRandom m, Foldable players) => players player -> m (Partner player)
requestPartner = fmap Partner . uniform

singularDependencies :: MonadRandom m => SingularDependencies m player
singularDependencies =
  SingularDependencies
    { requestBidSingular = requestBid
    , requestViceTrumpSingular = requestViceTrump
    , requestChiefTrumpSingular = requestChiefTrump
    , requestPartnerSingular = requestPartner
    , requestCardSingular = uniform
    }

moveUpdates :: Applicative f => MoveUpdates f player
moveUpdates =
  MoveUpdates
    { bidUpdate = doNothing
    , viceTrumpUpdate = doNothing
    , chiefTrumpUpdate = doNothing
    , partnerUpdate = doNothing
    , cardUpdate = doNothing
    }
  where
    doNothing _ _ = pure ()

singularUpdates :: Applicative f => SingularUpdates f player scores
singularUpdates =
  SingularUpdates
    { singularDealUpdate = doNothing
    , singularBiddingResultUpdate = doNothing
    , singularTrickWinnerUpdate = doNothing
    , singularScoresUpdate = doNothing
    }
  where
    doNothing _ = pure ()
