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

type alias Player = {cards: Array Card, score: Int, lock_flip: Bool}

type alias Model =
  {
     deck: Array Card
   , discard: Card
   , players: Array Player
   , n_players: Int
   , setting_up: Bool
   , hole: Int
   , turn: Int
  }

-- player size is hardcoded here right now as 2

init : () -> (Model, Cmd Msg)
init _ =
    (Model
        (Array.fromList []) 
        Cards.cardDefault
        (Array.fromList []) 
        2
        True
        0 -- this gets incremented one extra time to really start at 1
        0,
     Random.generate Deal (shuffle orderedDeck))

-- UPDATE

type Msg
  = Flip Int Int
  | Deal (List Card)
  | DeckClick

splitArray: Int -> Array a -> (Array a, Array a)
splitArray n arr = 
   let front = Array.slice 0 n arr in
   let back = Array.slice n (Array.length arr) arr in
       (front,back)


playerSettingUp: Player -> Bool
playerSettingUp player =
   (Array.foldl (+) 0 (Array.map (\c -> if c.show then 1 else 0) player.cards)) < 2

flipCard: Int -> Int -> Model -> Model
flipCard n_player n_card model = 
        -- this is an array of players
        let old_players = model.players in
        case Array.get n_player old_players of
            -- this is the player we want to flip a card for
            Just old_player ->
                 -- here, we need to decide is a flip is allowed
                 let n_showing = Array.foldl (+) 0 (Array.map (\c -> if c.show then 1 else 0) old_player.cards) in
                 let new_lock = n_showing >= 1 in
                 let old_lock = n_showing >= 2 in



                 if old_lock then model else
                 case Array.get n_card old_player.cards of
                    Just old_card -> 
                            let new_card = {old_card | show = not old_card.show} in
                            let new_player = {old_player | cards = Array.set n_card new_card old_player.cards, lock_flip = new_lock} in
                            let new_players = Array.set n_player new_player old_players in

                            let new_model = { model | players = new_players } in

                            -- check if all are locked and can exit setup stage
                           let any_setup = Array.map (playerSettingUp) new_model.players |> Array.toList |> (List.any (\x -> x)) in

                           {new_model | setting_up = any_setup} 

                    Nothing -> model
            Nothing -> model


dealHelper: (Array Player, Array Card) -> (Array Player, Array Card)
dealHelper tup = 
    case tup of
       (player_arr, deck) -> 
               let (new_player,new_deck) = splitArray 6 deck in
               (Array.push {cards = new_player, score = 0, lock_flip = False} player_arr, new_deck)


deal: Int -> ((Array Player, Array Card) -> (Array Player, Array Card)) -> (Array Player, Array Card) -> (Array Player, Array Card)
deal i f acc = 
        if i <= 0 then acc else deal (i-1) f (f acc)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Deal newDeck ->
        let deck_arr = Array.fromList newDeck in
        let (player,deck) = deal model.n_players dealHelper (Array.empty, deck_arr) in
        -- one more for the discard
        let (discard,final_deck) = splitArray 1 deck in

        case (Array.get 0 discard) of
           Just card -> (Model 
              final_deck
              {card | show = True}
              player
              model.n_players
              True
              (model.hole + 1)
              0,
            Cmd.none)
           Nothing -> Debug.todo "failed to make initial discard"

    Flip n_player n_card ->
      ( flipCard n_player n_card model
      , Cmd.none
      )

    -- currently moves to discard, but should really move to consideration area...
    DeckClick ->
      let (discard,deck) = splitArray 1 model.deck in
      case (Array.get 0 discard) of
          Just card -> ({model| deck = deck, discard = {card| show = True}}, Cmd.none)
          Nothing -> (model, Cmd.none)

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


positionPlayer: Int -> List (Attribute msg)
positionPlayer n =
        case n of
           0 ->   [
              style "position" "fixed"
            , style "bottom" "1%"
            , style "left" "45%"
            --, style "background-color" "orange"
            ] 
           1 ->   [
              style "position" "fixed"
            , style "top" "1%"
            , style "left" "45%"
            --, style "background-color" "orange"
            ] 
           2 ->   [
              style "position" "fixed"
            , style "right" "1%"
            , style "top" "40%"
            --, style "background-color" "orange"
            ] 
           3 ->   [
              style "position" "fixed"
            , style "top" "40%"
            , style "left" "1%"
            --, style "background-color" "orange"
            ] 
           _ -> [] 

viewPlayer: Int -> Player -> Html Msg
viewPlayer n_player player = 
      div
      (positionPlayer n_player)
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


scoreCol: (Card,Card) -> Int
scoreCol rows = 
    case rows of
    (top,bot) -> if top.face == bot.face then 0 else (Cards.cardVal bot)+(Cards.cardVal top)

scorePlayer: Player -> Int
scorePlayer player = 
        let top_row = Array.toList (Array.slice 0 3 player.cards) in
        let bot_row = Array.toList (Array.slice 3 6 player.cards) in
        let zip = List.map2 Tuple.pair top_row bot_row in
        let vals = List.map scoreCol zip in
        List.foldl (+) 0 vals

-- I need something to view cards independent of actions

viewDeck: Model -> Html Msg
viewDeck model = 
  let dummy = {face = Cards.Knight, suit = Cards.Spades, show = False} in

  div
  [ style "font-size" "10em"
    , style "user-select" "none"
    , style "line-height" "150px"
    , style "position" "fixed"
    , style "left" "45%"
    , style "top" "40%"
  ]
  [
  span [onClick DeckClick, style "color" "black"] 
       [ 
        text (if (Array.length model.deck > 0) then (cardText dummy) else "")
--      , text (String.fromInt (Array.length model.deck))
       ]
   ]

viewDiscard: Model -> Html Msg
viewDiscard model = 
        div
        [ style "font-size" "10em"
       , style "user-select" "none"
       , style "line-height" "150px"
       , style "position" "fixed"
       , style "left" "52%"
       , style "top" "40%"]
        [span [style "color" (cardColor model.discard)] [text (cardText model.discard)]]

view : Model -> Html Msg
view model =
  div []  
      (List.concat 
       [
            Array.toList (Array.indexedMap viewPlayer model.players)
          , [viewDeck model]
          , [viewDiscard model] --probably need another place on the board for cards under consideration
--          , (Array.toList (Array.map (\p -> text ("Player" ++ String.fromInt (scorePlayer p) ++ "...")) model.players))
          , [div [] [text (if model.setting_up then "setup..." else "setup done!")]]
          , [div [] [text ("\nHole: " ++ (String.fromInt model.hole))]]
          , [div [] [text ("\nTurn (mod n_players + 1): Player " ++ (String.fromInt ((modBy model.n_players model.turn)+1)))]]
       ]
      )


