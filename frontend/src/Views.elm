module Views exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Cards exposing (..)
import Array exposing (Array)
import Actions exposing (..)
import Model exposing (..)
import Stage exposing (..)

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
            ] 
           Top ->   [
              style "position" "fixed"
            , style "top" "1%"
            , style "left" "45%"
            ] 
           Right ->   [
              style "position" "fixed"
            , style "right" "1%"
            , style "top" "40%"
            ] 
           Left ->   [
              style "position" "fixed"
            , style "top" "40%"
            , style "left" "1%"
            ] 


viewPlayer: Int -> Model -> Html Msg
viewPlayer n_player model = 
      case (Array.get n_player model.players, Array.get n_player model.player_names) of
         (Just player,Just name) -> div  (n_player |> (getPos model) |> positionPlayer) [
                          div [
                              ]
                            [
                                viewCard n_player 0 player.cards "green"
                              , viewCard n_player 1 player.cards "red"
                              , viewCard n_player 2 player.cards "green"
                            ]
                        , div [
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


roomView : Model -> Html Msg
roomView model =
  div []
    [
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

-- ifIsEnter : msg -> D.Decoder msg
-- ifIsEnter msg =
--   D.field "key" D.string
--     |> D.andThen (\key -> if key == "Enter" then D.succeed msg else D.fail "some other key")


turnName: Model -> String
turnName model = 
  let idx = if model.n_players == 4 then
            case Array.get (modBy model.n_players model.turn) (Array.fromList [0, 3, 1, 2]) of
                Just x -> x
                Nothing -> Debug.todo "failed to get turn"
            else
              (modBy model.n_players model.turn)
  in

  case Array.get idx model.player_names of
    Just wsn -> wsn.name
    _ -> "" 

perspName: Int -> Model -> String
perspName p model = 
  case Array.get p model.player_names of
    Just wsn -> wsn.name
    _ -> "" 

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

