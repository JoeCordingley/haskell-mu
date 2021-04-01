module Auction where

import Prelude
import Cards (Card)
import Halogen as H
import Halogen.HTML as HH
import Data.Map (Map)
import Data.Set as Set
import Data.Set (Set)


component :: forall t27 t30 t7 t8. H.Component HH.HTML t30 t27 t8 t7
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval 
    }
  where
    initialState = identity
    render state =
      HH.div_ []

data Action card 
  = Pick card 
  | Unpick card
  | Bid

type State
  = { cards :: Map Int Card, max :: Int, picked :: Set Int }

type Input
  = { cards :: Map Int Card, max :: Int}

type Output = Set Int

--cardPicker:: forall query slots m. H.Component query Input Output m
cardPicker =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      --{ handleAction : handleAction }
    }
    where 
      initialState {max, cards} = {max: max, cards: cards, picked: Set.empty}
      render state =
        HH.div_ []
--      handleAction = case _ of
--        Pick card -> H.modify_ (addCard card)
--        Unpick card -> H.modify (removeCard card)
--        Bid -> H.gets _.picked >>= H.raise
--      addCard card = Set.insert card
--      removeCard card = Set.delete card

