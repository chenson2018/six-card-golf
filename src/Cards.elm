module Cards exposing (..)

import Random

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
  List.concat (List.map (\s -> List.map (\f -> {face = f, suit = s, show = True}) faces )suits)


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



