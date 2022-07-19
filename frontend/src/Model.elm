module Model exposing (..)

import Array exposing (Array)
import Cards exposing (..)
import Players exposing (..)
import Stage exposing (..)

import Json.Decode as D
import Json.Encode as Encode

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
   , player_names: Array {id: Int, name: String}
  }

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

