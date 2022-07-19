module Stage exposing (..)

import Json.Decode as D
import Json.Encode as Encode

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

