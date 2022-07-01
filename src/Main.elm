-- Press a button to draw a random card.
--
-- Dependencies:
--   elm install elm/random
--

module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Cards exposing (..)
import Random
import Array exposing (Array)
import Array

import Random.List exposing (shuffle)

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type alias Player = {cards: Array Card, score: Int}

type alias Model =
  {
     deck: Array Card
   , player: Array Player
   , n_player: Int
  }

-- player size is hardcoded here right now as 2

init : () -> (Model, Cmd Msg)
init _ =
    (Model
        (Array.fromList []) 
        (Array.fromList []) 
        2,
     Random.generate Deal (shuffle orderedDeck))

-- UPDATE

type Msg
  = Flip Int Int
  | Deal (List Card)

splitArray: Int -> Array a -> (Array a, Array a)
splitArray n arr = 
   let front = Array.slice 0 n arr in
   let back = Array.slice n (Array.length arr) arr in
       (front,back)


flipCard: Int -> Int -> Model -> Model
flipCard n_player n_card model = 
        -- this is an array of players
        let old_players = model.player in
        case Array.get n_player old_players of
            -- this is the player we want to flip a card for
            Just old_player ->
                 case Array.get n_card old_player.cards of
                    Just old_card -> 
                            let new_card = {old_card | show = not old_card.show} in
                            let new_player = {old_player | cards = Array.set n_card new_card old_player.cards} in
                            let new_players = Array.set n_player new_player old_players in
                            { model | player = new_players }
                    Nothing -> model
            Nothing -> model
          



--      let old_player = model.player in
--      let old_card = (Maybe.withDefault cardDefault (Array.get n old_player.cards)) in
--
--      let new_card = {old_card | show = not old_card.show} in
--      let new_player = {old_player| cards =  Array.set n new_card old_player.cards} in
--
--      {model| player = new_player }

dealHelper: (Array Player, Array Card) -> (Array Player, Array Card)
dealHelper tup = 
    case tup of
       (player_arr, deck) -> 
               let (new_player,new_deck) = splitArray 6 deck in
               (Array.push {cards = new_player, score = 0} player_arr, new_deck)


deal: Int -> ((Array Player, Array Card) -> (Array Player, Array Card)) -> (Array Player, Array Card) -> (Array Player, Array Card)
deal i f acc = 
        if i <= 0 then acc else deal (i-1) f (f acc)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Deal newDeck ->
        let deck_arr = Array.fromList newDeck in
        let (player,deck) = deal model.n_player dealHelper (Array.empty, deck_arr) in

           (Model 
              deck 
              player
              model.n_player,
            Cmd.none)

    Flip n_player n_card ->
      ( flipCard n_player n_card model
      , Cmd.none
      )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

viewCard: Int -> Int -> Array Card -> String -> Html Msg
viewCard n_player idx player color = 
  let card = (Maybe.withDefault cardDefault (Array.get idx player)) in

  span
   [ 
      onClick (Flip n_player idx)
    , style "font-size" "10em"
    , style "color" (cardColor card)
    , style "user-select" "none"
    , style "line-height" "150px"
--    , style "background-color" color
   ] 
   [ 
      text (cardText card) 
   ]      



viewPlayer: Int -> Player -> Html Msg
viewPlayer n_player player = 
      div [
--              style "position" "fixed"
--            , style "bottom" "1%"
--            , style "left" "45%"
--            , style "background-color" "orange"
             style "padding-bottom" "100px"
          ] 
  [
   div [
--           style "background-color" "orange"
       ]
     [
         viewCard n_player 0 player.cards "green"
       , viewCard n_player 1 player.cards "red"
       , viewCard n_player 2 player.cards "green"
     ]
 , div [
--         style "background-color" "yellow"
       ]
     [
         viewCard n_player 3 player.cards "red"
       , viewCard n_player 4 player.cards "green"
       , viewCard n_player 5 player.cards "red"
     ]
 ]


view : Model -> Html Msg
view model =
  div []  (Array.toList (Array.indexedMap viewPlayer model.player))




--          (Maybe.withDefault cardDefault (Array.get idx player))
--        (viewPlayer model.player.cards)
--      , div [
--              style "position" "fixed"
--            , style "top" "1%"
--            , style "left" "45%"
----            , style "background-color" "orange"
--          ] 
--          (viewPlayer model.player.cards)

