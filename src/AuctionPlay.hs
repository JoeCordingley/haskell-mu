module AuctionPlay
  ( Interactions(..)
  ) where

import           AuctionFunctions
import           Cards
import           Control.Monad.State.Lazy
import           Data.List.Index
import qualified Data.Map.Lazy            as Map
import           Data.Maybe

data Trumps
  = SingleTrump Trump
  | HigherLower Trump
                Trump

data TrumpsAndTeams =
  TrumpsAndTeams Trumps
                 Teams

data Teams
  = ChiefAlone Player
  | ChiefAndPartner Player
                    Player

data FinishedAuction
  = Successful TrumpsAndTeams
  | Unsuccessful Stalemate

data Interactions f = Interactions
  { getBid     :: Player -> [Card] -> f Bid
  , getTrump   :: Player -> [Trump] -> f Trump
  , getPartner :: Player -> [Player] -> f Player
  }

type NumberOfPlayers = Int

findOrEmptyList :: (Ord k) => k -> Map.Map k [a] -> [a]
findOrEmptyList = Map.findWithDefault []

--test :: StateT Int IO Int
--test = StateT x where
--  x i = do
--    putStrLn $ show i
--    return (i+1,i+1)
bidding ::
     Monad f
  => NumberOfPlayers
  -> (Player -> [Card] -> f Bid)
  -> [Player]
  -> StateT AuctionState f AuctionResult
bidding numberOfPlayers getBid (thisPlayer:nextPlayers) =  do
  state <- get
  case auctionStatus numberOfPlayers state of
    Finished result -> return result
    Unfinished -> do
      bid <- lift $ getBid thisPlayer cards
      modify $ auctionState thisPlayer bid
      bidding numberOfPlayers getBid nextPlayers
      where cards = findOrEmptyList thisPlayer $ cardsInHand state

--getTrumps :: Monad f => (Player -> [Trump] -> f Trump) -> [Trump] -> Winners -> f Trumps
--getTrumps getTrump (ChiefOnly chief) trumps = fmap SingleTrump $ getTrump chief trumps
--getTrumps getTrump (ChiefAndVice chief vice) = do
--  lower <- getTrump vice
--  higher <- getTrump chief
--  return $ HigherLower higher lower
--

auctionRound :: Monad f => Interactions f -> StateT AuctionState f FinishedAuction
auctionRound interactions = do
  players <- gets $ Map.keys . cardsInHand
  numberOfPlayers <- return $ length players
  result <- bidding numberOfPlayers (getBid interactions) (cycle players)
  finalState <- get
  case result of
    Result winners -> do
      trumps <- case winners of
        ChiefOnly chief -> lift . fmap SingleTrump $ getTrump interactions chief  chiefTrumps 
        ChiefAndVice chief vice -> do
          viceTrump <- lift $ getTrump interactions vice viceTrumps
          chiefTrump <- lift . getTrump interactions chief $ remove viceTrump chiefTrumps 
          return $ HigherLower chiefTrump viceTrump
            where
              viceTrumps = cardTrumps viceCards
              viceCards = findOrEmptyList vice $ cardsBid finalState
      teams <- if numberOfPlayers == 3 then return . ChiefAlone $ chief' else lift $ fmap (ChiefAndPartner chief') (getPartner interactions chief' $ potentialParters winners)
      return . Successful $ TrumpsAndTeams trumps teams
        where 
          chief' = chief winners
          potentialParters (ChiefOnly chief) = remove chief players
          potentialParters (ChiefAndVice chief vice) = remove chief $ remove vice players
          chiefTrumps = NoTrump : (cardTrumps chiefCards)
          chiefCards = findOrEmptyList chief' $ cardsBid finalState
    NoResult stalemate -> return $ Unsuccessful stalemate
--    bidding = undefined
--
--type NumberOfPlayers = Int

--auctionRound :: Monad f => Interactions f -> [Player] -> f FinishedAuction
--auctionRound Interactions { getBid = getBid
--                          , getTrump = getTrump
--                          , getPartner = getPartner
--                          } players = do
--  result <- bidding getBid playerSequence numberOfPlayers
--  case result of
--    Result winners -> do
--      trumps <- getTrumps getTrump winners
--      teams <- getTeams getPartner chief numberOfPlayers
--      return . Successful $ TrumpsAndTeams trumps teams
--      where chief = chiefOf winners
--    NoResult stalemate -> return $ Unsuccessful stalemate
--  where
--    playerSequence = cycle players
--    numberOfPlayers = length players
--bidding ::
--     Monad f
--  => (Player -> f Bid)
--  -> [Player]
--  -> NumberOfPlayers
--  -> f AuctionResult
--bidding getBid playerSequence numberOfPlayers = play playerSequence initialState
--  where
--    play = evalStateT . stateful
--    stateful (thisPlayer:nextPlayers) = do
--      status <- StateT $ placeBid numberOfPlayers getBid thisPlayer
--      case status of
--        Finished result -> return result
--        Unfinished      -> stateful nextPlayers
