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

type alias Model =
  {
     deck: List Card
   , player: {cards: Array Card, score: Int}
  }


init : () -> (Model, Cmd Msg)
init _ =
    (Model [] {cards = Array.fromList [], score = 0}, Random.generate Deal (shuffle orderedDeck))

-- UPDATE

type Msg
  = Flip Int
  | Deal (List Card)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Deal newDeck ->
      case newDeck of
         one::two::three::four::five::six::xs -> 
                 let player_cards = (Array.fromList [one, two, three, four, five, six]) in
                 (Model xs {cards = player_cards, score = 0}, Cmd.none)
         _ -> Debug.todo "invalid initial deck"
--                 (Model [] {cards = Array.fromList [], score = 0}, Cmd.none) -- should be unreachable, kinda nasty here... I really want a panic


    Flip n ->
      let old_player = model.player in
      let old_card = (Maybe.withDefault cardDefault (Array.get n old_player.cards)) in

      let new_card = {old_card | show = not old_card.show} in
      let new_player = {old_player| cards =  Array.set n new_card old_player.cards} in

      ( {model| player = new_player }
      , Cmd.none
      )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

viewCard: Int -> Array Card -> String -> Html Msg
viewCard idx player color = 
  let card = (Maybe.withDefault cardDefault (Array.get idx player)) in

  span
   [ 
      onClick (Flip idx)
    , style "font-size" "10em"
    , style "color" (cardColor card)
    , style "user-select" "none"
    , style "line-height" "150px"
--    , style "background-color" color
   ] 
   [ 
      text (cardText card) 
   ]      



viewPlayer: Array Card -> List (Html Msg)
viewPlayer player = 
  [
   div [
--           style "background-color" "orange"
       ]
     [
         viewCard 0 player "green"
       , viewCard 1 player "red"
       , viewCard 2 player "green"
     ]
 , div [
--         style "background-color" "yellow"
       ]
     [
         viewCard 3 player "red"
       , viewCard 4 player "green"
       , viewCard 5 player "red"
     ]
 ]

view : Model -> Html Msg
view model =
  div []
    [ 
      div [
              style "position" "fixed"
            , style "bottom" "1%"
            , style "left" "45%"
--            , style "background-color" "orange"
          ] 
          (viewPlayer model.player.cards)
      , div [
              style "position" "fixed"
            , style "top" "1%"
            , style "left" "45%"
--            , style "background-color" "orange"
          ] 
          (viewPlayer model.player.cards)
    ]

