{-# LANGUAGE NamedFieldPuns             #-}
module New.BiddingResolution where

import AuctionFunctions (Winners(..))
import New.Bidding (CardPositions)
import AuctionPlay (TrumpsAndPartner(..)) 


newtype IsMoreThanThreePlayers = IsMoreThanThreePlayers Bool

--settleAuctionRound (IsMoreThanThreePlayers isMoreThanThreePlayers) Winners { topBidder, vice } cardPositions = do 
--  let chief = topBidder
--  viceTrump <- traverse getViceTrump vice
--  chiefTrump <- getChiefTrump topBidder
--  partner <- if isMoreThanThreePlayers then fmap Just  (getPartner chief) else return Nothing
--  return TrumpsAndChiefsTeam {trumps = Trumps { chiefTrump, viceTrump }, partner}
--    where
--      getViceTrump = undefined
--      getChiefTrump = undefined
--      getPartner = undefined




