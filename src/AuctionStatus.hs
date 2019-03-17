module AuctionStatus where

import AuctionState


data AuctionStatus n
  = Unfinished
  | Finished ( AuctionResult n )
  deriving (Eq, Show)

data AuctionResult n
  = Result (Winners n)
  | NoResult (Stalemate n)
  deriving (Eq, Show)

data Winners n
  = ChiefOnly ( Player n )
  | ChiefAndVice ( Player n )
                 ( Player n )
  deriving (Eq, Show)

data Stalemate n
  = EklatNoPoints
  | Eklat { atFault  :: Player n
          , affected :: [Player n] }
  deriving (Eq, Show)

auctionStatus :: NumberOf n => AuctionState n -> AuctionStatus n
auctionStatus state = if passes state < numberOf state then Unfinished else undefined

num :: NumberOf n => AuctionState n -> Int
num s = numberOf s
