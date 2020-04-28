module BiddingResolution
  ( resolveBidding
  , ResolvedBidding(..)
  ) where

import           Cards

data ResolvedBidding player
  = EklatNoPoints
  | Chief player
  deriving (Eq, Show)

resolveBidding :: [(player, [Card])] -> ResolvedBidding player
resolveBidding _ = EklatNoPoints
