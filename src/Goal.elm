module Goal exposing (..)

import Coordinate exposing (Coordinate, toCoordinate)
import Json.Decode

-- Include Wildcard?
type GoalSymbol
  = RedMoon
  | GreenMoon
  | BlueMoon
  | YellowMoon
  | RedPlanet
  | GreenPlanet
  | BluePlanet
  | YellowPlanet
  | RedCross
  | GreenCross
  | BlueCross
  | YellowCross
  | RedGear
  | GreenGear
  | BlueGear
  | YellowGear

type alias Goal =
  { pos : Coordinate
  , symbol : GoalSymbol
  , active : Bool
  }
  
{-| Helper function to print Goal Symbols or get their filenames -}
toString : GoalSymbol -> { plaintext: String, filename: String }
toString goal =
  case goal of
    RedMoon -> { plaintext = "Red Moon", filename = "red-moon" }
    GreenMoon -> { plaintext = "Green Moon", filename = "green-moon" }
    BlueMoon -> { plaintext = "Blue Moon", filename = "blue-moon" }
    YellowMoon -> { plaintext = "Yellow Moon", filename = "yellow-moon" }
    RedPlanet -> { plaintext = "Red Planet", filename = "red-planet" }
    GreenPlanet -> { plaintext = "Green Planet", filename = "green-planet" }
    BluePlanet -> { plaintext = "Blue Planet", filename = "blue-planet" }
    YellowPlanet -> { plaintext = "Yellow Planet", filename = "yellow-planet" }
    RedCross -> { plaintext = "Red Cross", filename = "red-cross" }
    GreenCross -> { plaintext = "Green Cross", filename = "green-cross" }
    BlueCross -> { plaintext = "Blue Cross", filename = "blue-cross" }
    YellowCross -> { plaintext = "Yellow Cross", filename = "yellow-cross" }
    RedGear -> { plaintext = "Red Gear", filename = "red-gear" }
    GreenGear -> { plaintext = "Green Gear", filename = "green-gear" }
    BlueGear -> { plaintext = "Blue Gear", filename = "blue-gear" }
    YellowGear -> { plaintext = "Yellow Gear", filename = "yellow-gear" }

{-| Helper function: does (i, j) match position of Goal? -}
matchGoal : Int -> Int -> Goal -> Bool
matchGoal rowi colj record =
  record.pos == (colj, rowi)

{-| JSON decoder for Goal Symbol -}
decodeGoalSymbol : Json.Decode.Decoder GoalSymbol
decodeGoalSymbol =
  Json.Decode.string
    |> Json.Decode.andThen (\str ->
      case str of
        "RedMoon"      -> Json.Decode.succeed RedMoon
        "GreenMoon"    -> Json.Decode.succeed GreenMoon
        "BlueMoon"     -> Json.Decode.succeed BlueMoon
        "YellowMoon"   -> Json.Decode.succeed YellowMoon
        "RedPlanet"    -> Json.Decode.succeed RedPlanet
        "GreenPlanet"  -> Json.Decode.succeed GreenPlanet
        "BluePlanet"   -> Json.Decode.succeed BluePlanet
        "YellowPlanet" -> Json.Decode.succeed YellowPlanet
        "GreenCross"   -> Json.Decode.succeed GreenCross
        "RedCross"     -> Json.Decode.succeed RedCross
        "BlueCross"    -> Json.Decode.succeed BlueCross
        "YellowCross"  -> Json.Decode.succeed YellowCross
        "RedGear"      -> Json.Decode.succeed RedGear
        "GreenGear"    -> Json.Decode.succeed GreenGear
        "BlueGear"     -> Json.Decode.succeed BlueGear
        "YellowGear"   -> Json.Decode.succeed YellowGear
        somethingElse  -> Json.Decode.fail <| "Unknown Goal Symbol: " ++ somethingElse
    )

{-| JSON decoder for Goal object -}
decodeGoal : Json.Decode.Decoder Goal
decodeGoal =
  Json.Decode.map3
    Goal
    (Json.Decode.field "pos" (Json.Decode.map2 toCoordinate
      (Json.Decode.field "x" Json.Decode.int)
      (Json.Decode.field "y" Json.Decode.int)
    ))
    (Json.Decode.field "symbol" decodeGoalSymbol)
    (Json.Decode.field "active" Json.Decode.bool)
  
{-| JSON decoder for a *list* of Goal objects -}
decodeGoalList : Json.Decode.Decoder (List Goal)
decodeGoalList =
  Json.Decode.list decodeGoal
  