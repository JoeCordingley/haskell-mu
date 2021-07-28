module Auction where

import Prelude
import Cards (Card(..), Suit(..))
import Cards as Cards
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Maybe (Maybe(..))
import Halogen.HTML.CSS as CSS
import CSS.Stylesheet (StyleM)
import CSS.Border as Border
import CSS.Size as Size
import CSS.Color as Color
import Data.Array (mapMaybe)


data Action
  = Toggle Int 
  | Bid

type SelectableCard
  = { index:: Int, card:: Card , selected:: Boolean}

type IndexedCard
  = { index:: Int, card:: Card}

unselected :: IndexedCard -> SelectableCard
unselected {index, card} = {index: index, card: card, selected: false}

type State 
  = { cards :: Array SelectableCard, max :: Int}

type Input
  = { cards :: Array IndexedCard, max :: Int}

type Output = Array Int

cardPicker:: forall query m. H.Component HH.HTML query Input Output m
cardPicker =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction }
    }
    where 
      initialState {max, cards} = {max: max, cards: map unselected cards}


render :: forall w. State -> HH.HTML w Action
render state =
  HH.div_ (map cardPick state.cards) where
    cardPick { index, card, selected} = 
      HH.button [ HE.onClick \_ -> Just (Toggle index) , CSS.style (if selected then inset else none)] [Cards.render card]

handleAction :: forall slots m. Action -> H.HalogenM State Action slots Output m Unit
handleAction = case _ of
  (Toggle i) -> H.modify_ (toggleState i)
  Bid -> H.gets pickedCards >>= H.raise

toggleState :: Int -> State -> State
toggleState i state = state { cards = map (toggle i) state.cards }

toggle:: Int -> SelectableCard -> SelectableCard
toggle i card = case card of
  {index} | index == i -> card {selected = not card.selected}
  _ -> card

inset :: StyleM Unit
inset = Border.border Border.inset (Size.pt 2.0) Color.black

none :: StyleM Unit
none = Border.border Border.outset (Size.pt 2.0) Color.black

pickedCards :: State -> Array Int
pickedCards {cards} = mapMaybe indexIfSelected cards where
  indexIfSelected card = if card.selected then Just card.index else Nothing

someIndexedCards :: Array IndexedCard
someIndexedCards = 
  [ {index: 1, card : Card{ suit: Red, rank: 1 }}
  , {index: 2, card : Card{ suit: Blue, rank: 2 }}
  , {index: 3, card : Card{ suit: Green, rank: 3 }}
  ]
