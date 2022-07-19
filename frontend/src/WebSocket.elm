port module WebSocket exposing (..)

import Model exposing (..)
import Array exposing (Array)

import Json.Decode as D
import Json.Encode as Encode

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


