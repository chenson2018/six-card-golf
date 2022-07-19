module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Random
import Random.List exposing (shuffle)

import Cards exposing (..)
import Players exposing (..)
import WebSocket exposing (..)
import Model exposing (..)
import Stage exposing (..)
import Actions exposing (..)
import Views exposing (..)

import Array exposing (Array)
import Array

-- MAIN

init : () -> (Model, Cmd Msg)
init _ =
    (Model
        (Array.fromList []) 
        Cards.cardDefault
        (Array.fromList []) 
        0
        RoomSetup
        0 
        0
        0
        ""
        ""
        (Array.fromList []), Cmd.none)

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let new_model = updateModel msg model in
  case msg of
    -- these are actions from the player, where we subsequently send to the server and out to the other players
    DiscardClick -> update SendModel new_model
    Deal _       -> update SendModel new_model
    Flip _ _     -> update SendModel new_model
    DeckClick    -> update SendModel new_model

    -- bit of a special case, handling a deal
    ClickDeal -> (new_model , Random.generate Deal (shuffle orderedDeck))

    -- these only affect the indiovisual client
    DraftChanged draft -> (new_model, Cmd.none)
    Recv message       -> (new_model, Cmd.none)

    -- sending information to the websocket
    Send      -> (new_model, Cmd.batch [ sendEncode List, sendEncode (Name model.draft)])
    SendModel -> (new_model, sendEncode (ModelMessage model))

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver Recv

-- VIEW

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h3 [] [ text "Six Card Golf" ]
        , viewSelect model
        ]

viewSelect : Model -> Html Msg
viewSelect model =
  case model.stage of
   RoomSetup -> roomView model
   HoleSetup -> playView model
   Turns     -> playView model
   EndRound  -> playView model
   Considering -> playView model


