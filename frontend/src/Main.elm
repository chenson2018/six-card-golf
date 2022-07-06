port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Json.Decode as D
import Json.Encode as Encode

import Random
import Random.List exposing (shuffle)

import Cards exposing (..)
import Players exposing (..)
import Array exposing (Array)
import Array

-- MAIN

main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- PORTS

port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg

type WSMessage 
  = List
  | Name String
  | ModelMessage Model

sendEncode: WSMessage -> Cmd msg
sendEncode kind = 
  case kind of
    List   -> sendMessage (Encode.encode 0 (Encode.object [ ("kind", Encode.string "list") ]))
    Name s -> sendMessage (Encode.encode 0 (Encode.object [ ("kind", Encode.string "name"), ("name", Encode.string s) ]))
    ModelMessage model -> sendMessage (Encode.encode 0 (encodeModel model))

-- MODEL

type alias Model =
  {
     deck: Array Card
   , discard: Card
   , players: Array Player
   , n_players: Int
   , stage: Stage
   , hole: Int
   , turn: Int
   , perspective: Int -- this needs to be gotten from the websocket somehow????
   , draft : String
   , name : String
   , player_names: List WSName
  }


type Stage
  = RoomSetup
  | HoleSetup 
  | Turns

stageString: Stage -> String
stageString stage = 
  case stage of
    RoomSetup -> "RoomSetup"
    HoleSetup -> "HoleSetup"
    Turns -> "Turns"

decodeStage: D.Decoder Stage
decodeStage = D.string |>
  D.andThen
    (\str ->
      case str of
       "RoomSetup" -> D.succeed RoomSetup
       "HoleSetup" -> D.succeed HoleSetup
       "Turns"     -> D.succeed Turns
       _           -> D.fail "Invalid Stage"
    )


init : () -> (Model, Cmd Msg)
init _ =
    (Model
        (Array.fromList []) 
        Cards.cardDefault
        (Array.fromList []) 
        0
        RoomSetup
        0 -- this gets incremented one extra time to really start at 1
        0
        0
        ""
        ""
        [], Cmd.none)

encodeModel: Model -> Encode.Value
encodeModel model = 
  Encode.object
    [   ( "kind", Encode.string "model")
      , ( "deck", Encode.array encodeCard model.deck )
      , ( "discard", encodeCard model.discard )
      , ( "players", Encode.array encodePlayer model.players )
      , ( "stage", Encode.string (stageString model.stage) )
      , ( "hole", Encode.int model.hole )
      , ( "turn", Encode.int model.turn )
    ]

-- partial 
type alias PartialModel =
  {
     deck: Array Card
   , discard: Card
   , players: Array Player
   , stage: Stage
   , hole: Int
   , turn: Int
  }


fromPartial: Model -> PartialModel -> Model
fromPartial model p =
  { model | deck = p.deck, discard = p.discard, players = p.players, stage = p.stage, hole = p.hole, turn = p.turn }


decodePartial: D.Decoder PartialModel
decodePartial = 
  D.map6
    PartialModel
      (D.field "deck" (D.array decodeCard))
      (D.field "discard" decodeCard)
      (D.field "players" (D.array decodePlayer))
      (D.field "stage" decodeStage)
      (D.field "hole" D.int)
      (D.field "hole" D.int)

-- UPDATE

type Msg
  = Flip Int Int
  | Deal (List Card)
  | DeckClick
  | ClickDeal
  | DraftChanged String
  | Send
  | Recv String
  | SendModel

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
                            let new_card = {old_card | show = True } in
                            let new_player = {old_player | cards = Array.set n_card new_card old_player.cards, lock_flip = new_lock} in
                            let new_players = Array.set n_player new_player old_players in

                            let new_model = { model | players = new_players } in

                            -- check if all are locked and can exit setup stage
                           let any_setup = Array.map (playerSettingUp) new_model.players |> Array.toList |> (List.any (\x -> x)) in

                           {new_model | stage = (if any_setup then HoleSetup else Turns)} 

                    Nothing -> model
            Nothing -> model


type alias WSName = {id: Int, name: String}
type alias RustWSResponse2 = { kind: String, values: List WSName }


nameDecoder: D.Decoder WSName
nameDecoder = 
  D.map2
    WSName
      (D.field "id" D.int)
      (D.field "name" D.string)

websocketDecoder : D.Decoder RustWSResponse2
websocketDecoder =
    D.map2
        RustWSResponse2
        (D.field "kind" D.string)
        (D.field "values" (D.list nameDecoder))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    DraftChanged draft ->
      ( { model | draft = draft }
      , Cmd.none
      )

    Send ->
      ( { model | name = model.draft }
      , Cmd.batch [ sendEncode List, sendEncode (Name model.draft)]
      )

    -- this is meh, waterfall each json type that I can get from Rust
    Recv message ->
      let ws_json = D.decodeString websocketDecoder message in

      case ws_json of
        Ok json -> case json.kind of
                    "players" -> ({model| player_names = json.values, n_players = (List.length json.values)}, Cmd.none)
                    _ -> Debug.todo "didn't match a kind"
        _ -> let pmodel_json = D.decodeString decodePartial message in
             case pmodel_json of 
              Ok p -> (fromPartial model p, Cmd.none)
              _    -> Debug.todo "invalid json"

    SendModel -> (model, sendEncode (ModelMessage model))

    ClickDeal -> (
                  model, 
                  Random.generate Deal (shuffle orderedDeck)
                 )

    Deal newDeck ->
        let deck_arr = Array.fromList newDeck in
        let n_players = List.length model.player_names in
        let (players,deck) = deal n_players dealHelper (Array.empty, deck_arr) in
        -- one more for the discard
        let (discard,final_deck) = splitArray 1 deck in

        case (Array.get 0 discard) of
          Just card -> let m = Model final_deck {card | show = True} players n_players HoleSetup (model.hole + 1) 0 model.perspective model.draft model.name model.player_names in
                       update SendModel m
          Nothing -> Debug.todo "failed to make initial discard"

    Flip n_player n_card ->
      let m = flipCard n_player n_card model in
      update SendModel m

    -- currently moves to discard, but should really move to consideration area...
    DeckClick ->
      let (discard,deck) = splitArray 1 model.deck in
      case (Array.get 0 discard) of
          Just card -> ({model| deck = deck, discard = {card| show = True}}, Cmd.none)
          Nothing -> (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  messageReceiver Recv

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


type Position = 
   Top
 | Bot
 | Left
 | Right

positionPlayer: Position -> List (Attribute msg)
positionPlayer pos =
        case pos of
           Bot ->   [
              style "position" "fixed"
            , style "bottom" "1%"
            , style "left" "45%"
            --, style "background-color" "orange"
            ] 
           Top ->   [
              style "position" "fixed"
            , style "top" "1%"
            , style "left" "45%"
            --, style "background-color" "orange"
            ] 
           Right ->   [
              style "position" "fixed"
            , style "right" "1%"
            , style "top" "40%"
            --, style "background-color" "orange"
            ] 
           Left ->   [
              style "position" "fixed"
            , style "top" "40%"
            , style "left" "1%"
            --, style "background-color" "orange"
            ] 

viewPlayer: Int -> Model -> Html Msg
viewPlayer n_player model = 
      case Array.get n_player model.players of
         Just player -> div  (n_player |> (getPos model) |> positionPlayer) [
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
                        , div [] [text ("Player " ++ (String.fromInt n_player))]

                 ]
         Nothing -> div [] [] --weird case at begining when deal hasn't happened yet


-- this logic is very messy....

getPos: Model -> Int -> Position
getPos model n_player = 
        case model.n_players of
            2 -> if modBy 2 (n_player + model.perspective) == 0 then Bot else Top
            3 ->  case model.perspective of
                    0 -> if n_player == 0 then Bot  else if n_player == 1 then Top   else Right
                    1 -> if n_player == 0 then Top  else if n_player == 1 then Bot   else Left
                    2 -> if n_player == 0 then Left else if n_player == 1 then Right else Bot
                    _ -> Debug.todo "invalid perspective"
            4 ->  case model.perspective of
                    0 -> if n_player == 0 then Bot   else if n_player == 3 then Left  else if n_player == 1 then Top   else Right
                    1 -> if n_player == 0 then Top   else if n_player == 3 then Right else if n_player == 1 then Bot   else Left
                    2 -> if n_player == 0 then Left  else if n_player == 3 then Top   else if n_player == 1 then Right else Bot
                    3 -> if n_player == 0 then Right else if n_player == 3 then Bot   else if n_player == 1 then Left  else Top
                    _ -> Debug.todo "invalid perspective"
            _ -> Debug.todo ((String.fromInt model.n_players) ++ " players not implemented")

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
    div [ class "container" ]
        [ h3 [] [ text "Six Card Golf" ]
        , viewSelect model
        ]


roomView : Model -> Html Msg
roomView model =
  div []
    [
        div [] [text ("Draft: " ++ model.draft)]
      , div [] [text ("Stage: "  ++ (stageString model.stage))]
      , div [] [text ("Name: "  ++ model.name )]
      , div [] [text ("Currently connected players: " ++ (String.join ", " (List.map (\x -> x.name) model.player_names)))]
      , if String.isEmpty model.name then
        div []
        [
           input
           [ type_ "text"
           , placeholder "Enter a name"
           , onInput DraftChanged
   --        , on "keydown" (ifIsEnter Send)
           , value model.draft
           ]
           []
          ,  button [ onClick Send] [ text "Submit" ]
        ]
      else  
        button [ onClick ClickDeal ] [ text "Begin game" ]
    ]

ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
  D.field "key" D.string
    |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")

viewSelect : Model -> Html Msg
viewSelect model =
  case model.stage of
   RoomSetup -> roomView model
   HoleSetup -> playView model
   Turns     -> playView model

playView : Model -> Html Msg
playView model =
  div []  
      (List.concat 
       [
            List.map (\n_player -> viewPlayer n_player model) (List.range 0 (model.n_players - 1))
          , [viewDeck model]
          , [viewDiscard model] --probably need another place on the board for cards under consideration
          , [div [] [text ("Stage: "  ++ (stageString model.stage))]]
          , [div [] [text ("\nPlaying as: " ++ "Player " ++ (String.fromInt model.perspective))]]
          , [div [] [text ("\nHole: " ++ (String.fromInt model.hole))]]
          , [div [] [text ("\nTurn: " ++ (String.fromInt model.turn))]]
--          , [div [] [text ("\nTurn: Player " ++ (String.fromInt (modBy model.n_players model.turn)))]]
          , [div [] [text ("Cards remaining in deck: " ++ (String.fromInt (Array.length model.deck)))]]
          , (Array.toList (Array.indexedMap (\i -> \p -> div [] [text ("Player " ++ (String.fromInt i) ++ " Score: " ++ String.fromInt (scorePlayer p))]) model.players))
       ]
      )


