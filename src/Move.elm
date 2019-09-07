module Move exposing (..)

import Color exposing (Color)
import List
import String
import Maybe
import Set

type Direction
  = Left
  | Up
  | Down
  | Right

type alias Move =
  { color : Color
  , direction : Direction
  }

directionToString : Direction -> String
directionToString dir =
  case dir of
    Left -> "left"
    Up -> "up"
    Down -> "down"
    Right -> "right"

countMoves : List Move -> Int
countMoves moves =
  List.length moves

countRobots : List Move -> Int
countRobots moves =
  List.map .color moves
  |> List.map Just
  |> List.map Color.toString
  |> Set.fromList
  |> Set.size