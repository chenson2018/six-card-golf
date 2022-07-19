module Cards exposing (..)

import Random
import Json.Encode as Encode
import Json.Decode as D

type Face
  = Ace
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | Knight
  | King


type Suit 
  = Spades
  | Hearts
  | Diamonds
  | Clubs

suits: List Suit
suits = [Spades, Hearts, Diamonds, Clubs]

faces: List Face
faces = 
    [ Ace
    , Two
    , Three
    , Four
    , Five
    , Six
    , Seven
    , Eight
    , Nine
    , Ten
    , Jack
    , Queen
    , King
    ]

type alias Card = {face: Face, suit: Suit, show: Bool}

-- this is a static full deck of cards

orderedDeck: List Card
orderedDeck =
  List.concat (List.map (\s -> List.map (\f -> {face = f, suit = s, show = False}) faces )suits)


cardDefault: Card
cardDefault = {face = Knight, suit = Spades, show = True}

cardGenerator : Random.Generator Card
cardGenerator =
  case orderedDeck of
     h::tl -> Random.uniform h tl
     [] -> Random.uniform cardDefault []   --this should be unreachable, putting knight as a warning


faceOffset: Face -> Int
faceOffset face = 
  case face of
    Ace -> 1
    Two -> 2
    Three ->3
    Four -> 4 
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 11
    Knight -> 12
    Queen -> 13
    King -> 14


cardVal: Card -> Int
cardVal card = 
  case card.face of
    Ace -> 1
    Two -> -2
    Three ->3
    Four -> 4 
    Five -> 5
    Six -> 6
    Seven -> 7
    Eight -> 8
    Nine -> 9
    Ten -> 10
    Jack -> 10
    Knight -> 999
    Queen -> 10
    King -> 0


suitOffset: Suit -> Int
suitOffset suit = 
  case suit of 
    Spades -> 0
    Hearts -> 1
    Diamonds -> 2
    Clubs -> 3
       
cardText : Card -> String
cardText card =
   let base = 0x1F0A0 in
   
   let hex = case card.show of
         True -> let suit = suitOffset card.suit in
                 let face = faceOffset card.face in
                 base+face+16*suit
         False -> base
   in

    Char.fromCode hex |> String.fromChar

cardColor: Card -> String
cardColor card = 
  if not card.show 
  then "black"
  else case card.suit of 
    Spades -> "black"
    Hearts -> "red"
    Diamonds -> "red"
    Clubs -> "black"


cardHref: Card -> String
cardHref card = 
  let base = "/assets/svg-cards.svg#" in

  if not card.show then
    base ++ "back"    
  else
    let suit = suitString card.suit in
    let face = faceString card.face in

    base ++ suit ++ "_" ++ face

faceString: Face -> String
faceString face = 
  case face of 
    Ace    -> "1"
    Two    -> "2"
    Three  -> "3"
    Four   -> "4"
    Five   -> "5"
    Six    -> "6"
    Seven  -> "7"
    Eight  -> "8"
    Nine   -> "9"
    Ten    -> "10"
    Jack   -> "jack"
    Queen  -> "queen"
    Knight -> "knight"
    King   -> "king"

suitString: Suit -> String
suitString suit =
  case suit of
    Spades   -> "spade"
    Hearts   -> "heart"
    Diamonds -> "diamond"
    Clubs    -> "club"

encodeCard: Card -> Encode.Value
encodeCard card = 
  let face = faceString card.face in
  let suit = suitString card.suit in
  Encode.object [ ("suit", Encode.string suit), ("face", Encode.string face), ("show", Encode.bool card.show) ]

decodeFace: D.Decoder Face
decodeFace = D.string |>
  D.andThen
    (\str ->
      case str of
       "1"      -> D.succeed Ace
       "2"      -> D.succeed Two
       "3"      -> D.succeed Three
       "4"      -> D.succeed Four
       "5"      -> D.succeed Five
       "6"      -> D.succeed Six
       "7"      -> D.succeed Seven
       "8"      -> D.succeed Eight
       "9"      -> D.succeed Nine
       "10"     -> D.succeed Ten
       "jack"   -> D.succeed Jack
       "queen"  -> D.succeed Queen
       "knight" -> D.succeed Knight
       "king"   -> D.succeed King
       _        -> D.fail "Invalid Suit"
    )

decodeSuit: D.Decoder Suit
decodeSuit = D.string |>
  D.andThen
    (\str ->
      case str of
       "spade"   -> D.succeed Spades
       "heart"   -> D.succeed Hearts
       "diamond" -> D.succeed Diamonds
       "club"    -> D.succeed Clubs
       _          -> D.fail "Invalid Suit"
    )

decodeCard: D.Decoder Card
decodeCard = 
  D.map3
    Card
      (D.field "face" decodeFace)
      (D.field "suit" decodeSuit)
      (D.field "show" D.bool)


scoreCol: (Card,Card) -> Int
scoreCol rows = 
    case rows of
    (top,bot) -> if top.face == bot.face then 0 else (cardVal bot)+(cardVal top)


