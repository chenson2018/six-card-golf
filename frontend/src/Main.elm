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
   , perspective: Int
   , draft : String
   , name : String
   , player_names: Array WSName
  }


type Stage
  = RoomSetup
  | HoleSetup 
  | Considering
  | Turns
  | EndRound

stageString: Stage -> String
stageString stage = 
  case stage of
    RoomSetup -> "RoomSetup"
    HoleSetup -> "HoleSetup"
    Turns -> "Turns"
    EndRound -> "EndRound"
    Considering -> "Considering"

decodeStage: D.Decoder Stage
decodeStage = D.string |>
  D.andThen
    (\str ->
      case str of
       "RoomSetup" -> D.succeed RoomSetup
       "HoleSetup" -> D.succeed HoleSetup
       "Turns"     -> D.succeed Turns
       "EndRound"  -> D.succeed EndRound
       "Considering" -> D.succeed Considering
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
        (Array.fromList []), Cmd.none)

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
      (D.field "turn" D.int)

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
  | DiscardClick

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

isTurn: Model -> Bool
isTurn model = 
  (modBy model.n_players model.turn) == model.perspective

doTurn: Int -> Int -> Model -> Model
doTurn n_player n_card model = 
  -- check that it is the player's turn
  if (isTurn model) then
    -- just handling the case where we clicked on one of our cards
    let old_players = model.players in

    case Array.get n_player old_players of
      Just old_player -> case Array.get n_card old_player.cards of
                           Just old_card ->   let new_card = {old_card | show = True } in
                                              let new_player = {old_player | cards = Array.set n_card model.discard old_player.cards} in
                                              let new_players = Array.set n_player new_player old_players in

                                              -- check if a player has flipped over all cards
                                              let end_round = List.any allUp (Array.toList new_players) in

                                              if end_round then
                                                {model | players = appendScore new_players, discard = new_card, turn = model.turn + 1, stage = EndRound }
                                              else 
                                                {model | players = new_players, discard = new_card, turn = model.turn + 1, stage = Turns }
                           Nothing -> model
      Nothing -> model

  else
    model

type alias WSName = {id: Int, name: String}
type alias RustWSResponse2 = { kind: String, values: Array WSName }


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
        (D.field "values" (D.array nameDecoder))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    DiscardClick -> 
      if not (isTurn model) then
        (model, Cmd.none)
      else
        case model.stage of
          Considering -> let (new_card,new_deck) = splitArray 1 model.deck in
                         case (Array.get 0 new_card) of
                           Just top -> let m = {model| discard = top, deck = new_deck, turn = model.turn + 1, stage = Turns} in
                                       update SendModel m
                           Nothing -> (model, Cmd.none)
          _ -> (model, Cmd.none)

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
                    "players" -> let player_names = json.values in
                                 let perps = Array.indexedMap Tuple.pair player_names in
                                 let me = Array.filter (\w -> (Tuple.second w).name == model.name) perps in   -- TODO use id here somehow or show error....
                                 case Array.get 0 me of
                                   Just hd -> ({model| player_names = player_names, n_players = (Array.length player_names), perspective = Tuple.first hd}, Cmd.none)
                                   _ -> Debug.todo "no matching names from websocket???"
                    _ -> Debug.todo "didn't match a kind"
        _ -> let pmodel_json = D.decodeString decodePartial message in
             case pmodel_json of 
              Ok p -> (fromPartial model p, Cmd.none)
              _    -> Debug.todo "invalid json"

    SendModel -> (model, sendEncode (ModelMessage model))

    ClickDeal -> (model , Random.generate Deal (shuffle orderedDeck))

    Deal newDeck ->
        let old_players = model.players in

        let deck_arr = Array.fromList newDeck in
        let n_players = Array.length model.player_names in
        let (players,deck) = deal n_players dealHelper (Array.empty, deck_arr) in
        -- one more for the discard
        let (discard,final_deck) = splitArray 1 deck in

        let final_players = if model.hole == 0 then
                              players else
                            Array.fromList (List.map2 (\o -> \n -> {n | score = o.score}) (Array.toList old_players) (Array.toList players))
        in

        case (Array.get 0 discard) of
          Just card -> let m = Model final_deck {card | show = True} final_players  n_players HoleSetup (model.hole + 1) (model.hole) model.perspective model.draft model.name model.player_names in
                       update SendModel m
          Nothing -> Debug.todo "failed to make initial discard"

    Flip n_player n_card ->
      let new_model = case model.stage of
                        HoleSetup   -> if model.perspective == n_player then
                                         flipCard n_player n_card model
                                       else 
                                         model
                        Turns       -> if model.perspective == n_player then
                                         doTurn n_player n_card model
                                       else
                                         model
                        Considering -> if (isTurn model) then
                                         let (new_card,new_deck) = splitArray 1 model.deck in
                                         let old_players = model.players in

                                         case (Array.get 0 new_card) of
                                            Just nc ->
                                               case Array.get n_player old_players of
                                                 Just old_player -> case Array.get n_card old_player.cards of
                                                                      Just old_card -> let new_player = {old_player | cards = Array.set n_card nc old_player.cards} in
                                                                                       let new_players = Array.set n_player new_player old_players in
                                                                                       let end_round = List.any allUp (Array.toList new_players) in

                                                                                       if end_round then
                                                                                       {model| deck = new_deck, players = appendScore new_players, stage = EndRound, turn = model.turn + 1, discard = {old_card|show=True}}
                                                                                       else 
                                                                                       {model| deck = new_deck, players = new_players, stage = Turns, turn = model.turn + 1, discard = {old_card|show=True}}
                                    
                                                                      Nothing -> model
                                                 Nothing -> model
                                            _ -> model
                                       else
                                         model
                        _ -> model
      in
      update SendModel new_model

    DeckClick ->
      if (isTurn model) then
        case model.stage of 
          Turns -> case Array.get 0 model.deck of
                      Just top_card -> let m = {model | deck = (Array.set 0 {top_card | show = True} model.deck), stage = Considering} in
                                       update SendModel m
                      _ -> (model, Cmd.none)
          _ -> (model, Cmd.none)
      else
        (model, Cmd.none)

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
      case (Array.get n_player model.players, Array.get n_player model.player_names) of
         (Just player,Just name) -> div  (n_player |> (getPos model) |> positionPlayer) [
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
                        , div [] [text name.name]

                 ]
         _ -> div [] [] --weird case at begining when deal hasn't happened yet


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


-- I need something to view cards independent of actions

viewDeck: Model -> Html Msg
viewDeck model = 
  case Array.get 0 model.deck of
    Just card ->  div
                  [ style "font-size" "10em"
                    , style "user-select" "none"
                    , style "line-height" "150px"
                    , style "position" "fixed"
                    , style "left" "45%"
                    , style "top" "40%"
                  ]
                  [
                  span [onClick DeckClick, style "color" (if card.show then (cardColor card) else "black")] 
                       [ 
                        text (if (Array.length model.deck > 0) then (cardText card) else "")
                       ]
                   ]
    _ -> div [] []

viewDiscard: Model -> Html Msg
viewDiscard model = 
        div
        [ style "font-size" "10em"
       , style "user-select" "none"
       , style "line-height" "150px"
       , style "position" "fixed"
       , style "left" "calc(45% + 125px)"
       , style "top" "40%"]
        [span [onClick DiscardClick, style "color" (cardColor model.discard)] [text (cardText model.discard)]]


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
--        div [] [text ("Draft: " ++ model.draft)]
--      , div [] [text ("Stage: "  ++ (stageString model.stage))]
--      , div [] [text ("Name: "  ++ model.name )]
        div [] [text ("Currently connected players: " ++ (String.join ", " (Array.toList (Array.map (\x -> x.name) model.player_names))))]
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
          , button [ onClick Send] [ text "Submit" ]
        ]
      else  
        if (Array.length model.player_names > 1) then
          button [ onClick ClickDeal ] [ text "Begin game" ]
        else 
          div [] []
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
   EndRound  -> playView model
   Considering -> playView model

turnName: Model -> String
turnName model = 
  case Array.get (modBy model.n_players model.turn) model.player_names of
    Just wsn -> wsn.name
    _ -> "" 

perspName: Int -> Model -> String
perspName p model = 
  case Array.get p model.player_names of
    Just wsn -> wsn.name
    _ -> "" 


appendScore: Array Player -> Array Player
appendScore players = 
  Array.map (\p -> {p | score = (scorePlayer p) :: p.score, cards = Array.map (\c -> {c|show=True}) p.cards}) players

playView : Model -> Html Msg
playView model =
  let n_holes = 9 in
  let current = (List.repeat (n_holes-model.hole+(if model.stage == EndRound then 0 else 1)) [td [] [text ""]]) in

  div []  
      (List.concat 
       [
            List.map (\n_player -> viewPlayer n_player model) (List.range 0 (model.n_players - 1))
          , [viewDeck model]
          , [viewDiscard model] 
          , [div [] [text ("\nHole: " ++ (String.fromInt model.hole))]]
          , [div [] [text ("\nTurn: " ++ (turnName model))]]
          , [
             table 
             [] 
             (List.concat [
                    [tr [] (List.concat [ [th [] [text "Hole"]], List.map (\i -> td [] [text (String.fromInt (i+1))]) (List.range 0 (n_holes-1)), [td [] [text "Total"]]  ])]
                  , Array.toList (Array.indexedMap (\i -> \p -> tr [] (List.concat [[th [] [text (perspName i model)]], List.map (\s -> td [] [text (String.fromInt s)] ) (List.reverse p.score), List.concat current, [td [] [text (String.fromInt (List.sum p.score))]] ]) ) model.players)
             ])
            ]
          , if (model.stage == EndRound && model.hole < n_holes) then [button [ onClick ClickDeal ] [ text "Start next hole" ]] else []
       ]
      )

