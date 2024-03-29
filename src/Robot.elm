module Robot exposing (Robot, decodeRobotsList, getByColor, matchRobot)

import Color
import Coordinate
import Board
import Move exposing (Direction(..))

import Json.Decode

type alias Robot =
  { pos : Coordinate.Coordinate
  , color : Color.Color
  , moves : List Direction
  }

{-| Get Robot of corresponding Color -}
getByColor : Color.Color -> List Robot -> Maybe Robot
getByColor color robots =
  let
    matchRobotByColor robot =
      robot.color == color
  in  
    (List.head (List.filter matchRobotByColor robots))

{--
{-| Return color of selected Robot -}
getColor : Maybe Robot -> Maybe Color.Color
getColor robot =
  Maybe.map .color robot
--}

{-| Helper function: does (colj, rowi) match position of Robot? -}
matchRobot : Int -> Int -> Robot -> Bool
matchRobot rowi colj robot =
  robot.pos == (colj, rowi)

{-| JSON decoder for Robot object -}
decodeRobot : Json.Decode.Decoder Robot
decodeRobot =
  Json.Decode.map3
    Robot
    (Json.Decode.map
      Coordinate.idxToCoordinate
      (Json.Decode.field "pos" Json.Decode.int)
    )
    (Json.Decode.field "color" Color.decodeColorSymbol)
    (Json.Decode.field "moves" Board.decodeDirectionsList)
  
{-| JSON decoder for list of Robot objects -}
decodeRobotsList : Json.Decode.Decoder (List Robot)
decodeRobotsList =
  Json.Decode.list decodeRobot
