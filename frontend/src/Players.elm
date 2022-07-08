module Players exposing (..)

import Json.Encode as Encode
import Json.Decode as D

import Array exposing (Array)
import Cards exposing (..)

type alias Player = {cards: Array Card, score: List Int, lock_flip: Bool}

encodePlayer: Player -> Encode.Value
encodePlayer player = 
   Encode.object [  ("cards", Encode.array encodeCard player.cards)
                  , ("score", Encode.list Encode.int player.score)
                  , ("lock_flip", Encode.bool player.lock_flip) ]


decodePlayer: D.Decoder Player
decodePlayer = 
  D.map3
    Player 
      (D.field "cards" (D.array decodeCard))
      (D.field "score" (D.list D.int))
      (D.field "lock_flip" D.bool)

playerSettingUp: Player -> Bool
playerSettingUp player =
   (Array.foldl (+) 0 (Array.map (\c -> if c.show then 1 else 0) player.cards)) < 2

splitArray: Int -> Array a -> (Array a, Array a)
splitArray n arr = 
   let front = Array.slice 0 n arr in
   let back = Array.slice n (Array.length arr) arr in
       (front,back)

dealHelper: (Array Player, Array Card) -> (Array Player, Array Card)
dealHelper tup = 
    case tup of
       (player_arr, deck) -> 
               let (new_player,new_deck) = splitArray 6 deck in
               (Array.push {cards = new_player, score = [], lock_flip = False} player_arr, new_deck)

allUp: Player -> Bool
allUp player = 
  let facedown = Array.filter (\c -> not c.show ) player.cards in
  Array.isEmpty facedown

deal: Int -> (a -> a) -> a -> a
deal i f acc = 
        if i <= 0 then acc else deal (i-1) f (f acc)


scorePlayer: Player -> Int
scorePlayer player = 
        let c1 = case (Array.get 0 player.cards, Array.get 3 player.cards) of
                   (Just t,Just b) -> scoreCol (t,b)
                   _ -> 99999
        in

        let c2 = case (Array.get 1 player.cards, Array.get 4 player.cards) of
                   (Just t,Just b) -> scoreCol (t,b)
                   _ -> 99999
        in

        let c3 = case (Array.get 2 player.cards, Array.get 5 player.cards) of
                   (Just t,Just b) -> scoreCol (t,b)
                   _ -> 99999
        in

        c1+c2+c3
