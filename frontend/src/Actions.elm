module Actions exposing (..)

import Model exposing (..)
import Players exposing (..)
import Stage exposing (..)
import Cards exposing (..)
import WebSocket exposing (..)

import Array
import Json.Decode as D

isTurn: Model -> Bool
isTurn model = 
  if model.n_players == 4 then
    case Array.get (modBy model.n_players model.turn) (Array.fromList [0, 3, 1, 2]) of
      Just x -> x == model.perspective
      Nothing -> Debug.todo "failed to get turn"
  else
    (modBy model.n_players model.turn) == model.perspective


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

updateModel: Msg -> Model -> Model
updateModel msg model =
  case msg of
    SendModel -> model

    ClickDeal -> model 

    DiscardClick ->
      if not (isTurn model) then
        model
      else
        case model.stage of
          Considering -> let (new_card,new_deck) = splitArray 1 model.deck in
                         case (Array.get 0 new_card) of
                           Just top -> {model | discard = top, deck = new_deck, turn = model.turn + 1, stage = Turns}
                           Nothing  -> model
          _ -> model

    DraftChanged draft -> { model | draft = draft }

    Send -> { model | name = model.draft }

    Recv message ->
      let ws_json = D.decodeString websocketDecoder message in

      case ws_json of
        Ok json -> case json.kind of
                    "players" -> let player_names = json.values in
                                 let perps = Array.indexedMap Tuple.pair player_names in
                                 let me = Array.filter (\w -> (Tuple.second w).name == model.name) perps in   -- TODO use id here somehow or show error....
                                 case Array.get 0 me of
                                   Just hd -> {model| player_names = player_names, n_players = (Array.length player_names), perspective = Tuple.first hd}
                                   _ -> Debug.todo "no matching names from websocket???"
                    _ -> Debug.todo "didn't match a kind"
        _ -> let pmodel_json = D.decodeString decodePartial message in
             case pmodel_json of 
              Ok p -> fromPartial model p
              _    -> Debug.todo "invalid json"


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
          Just card -> Model final_deck {card | show = True} final_players  n_players HoleSetup (model.hole + 1) (model.hole) model.perspective model.draft model.name model.player_names
          Nothing -> Debug.todo "failed to make initial discard"

    Flip n_player n_card ->
      case model.stage of
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

    DeckClick ->
      if (isTurn model) then
        case model.stage of 
          Turns -> case Array.get 0 model.deck of
                      Just top_card -> {model | deck = (Array.set 0 {top_card | show = True} model.deck), stage = Considering}
                      _ -> model
          _ -> model
      else
        model
  

