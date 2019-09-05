module Direction exposing (..)

type Direction
  = Left
  | Up
  | Down
  | Right

toString : Direction -> String
toString dir =
  case dir of
    Left -> "left"
    Up -> "up"
    Down -> "down"
    Right -> "right"
