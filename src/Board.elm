module Board exposing (..)

import Coordinate exposing (Coordinate)
import Move exposing (Direction(..))

import Array exposing (Array)
import Maybe
import Json.Decode


type alias Grid a =
  Array ( Array a)

column : Int -> Grid a -> Maybe (Array a)
column index grid =
  let
    got = Array.map (Array.get index) grid
  in
    got
      |> Array.map (Maybe.map (\x -> Array.fromList [ x ]))
      |> Array.map (Maybe.withDefault Array.empty)
      |> Array.foldr (Array.append) Array.empty
      |> Just


{-| Fetch the row at the given index.
    row 3 (repeat 1 4 "bun") == Just (Array.fromList ["bun"])
-}
row : Int -> Grid a -> Maybe (Array a)
row index grid =
  Array.get index grid

{-| Any function that can take two integers and return the desired Grid type -}
type alias Filler a =
  Int -> Int -> a


{-| Create a grid `width` units by `height` units, filling each cell according
to the cell's Coordinate.
    get (2, 1) (rectangle 4 2 (+)) == Just 3
-}
rectangle : Int -> Int -> Filler a -> Grid a
rectangle width height filler =
  Array.initialize height (\y -> Array.initialize width (\x -> filler x y))


{-| Like `rectangle`, except always make a square grid
-}
square : Int -> Filler a -> Grid a
square size filler =
  rectangle size size filler


{-| Fetch the cell at a given `Coordinate`. -}
get : Coordinate -> Grid a -> Maybe a
get coord grid =
  let
    a1 = Array.get (Coordinate.toRow coord) grid
  in
    case a1 of
      Nothing ->
        Nothing
    
      Just val ->
        Array.get (Coordinate.toColumn coord) val


{-| Overwrite the contents at a given `Coordinate` -}
set : Coordinate -> a -> Grid a -> Grid a
set coord occupant grid =
  row (Coordinate.toRow coord) grid
    |> Maybe.map (Array.set (Coordinate.toColumn coord) occupant)
    |> Maybe.map (\r -> Array.set (Coordinate.toRow coord) r grid)
    |> Maybe.withDefault grid


map : (a -> b) -> Grid a -> Grid b
map f grid =
  Array.map (Array.map f) grid


mapWithCoordinate : (Coordinate -> a -> b) -> Grid a -> Grid b
mapWithCoordinate f grid =
  Array.indexedMap
    (\y -> Array.indexedMap (\x -> f (Coordinate.toCoordinate x y)))
    grid

{-| JSON decoder for JSON arrays of arrays to a Grid -}
decodeBoard : Json.Decode.Decoder (Grid Int)
decodeBoard =
  Json.Decode.array decodeRow

decodeRow : Json.Decode.Decoder (Array Int)
decodeRow =
  Json.Decode.array Json.Decode.int


{-| JSON decoder for Direction -}
decodeDirection : Json.Decode.Decoder Direction
decodeDirection =
    Json.Decode.string
        |> Json.Decode.andThen (\str ->
           case str of
                "up"         -> Json.Decode.succeed Up
                "down"       -> Json.Decode.succeed Down
                "left"        -> Json.Decode.succeed Left
                "right"      -> Json.Decode.succeed Right
                somethingElse -> Json.Decode.fail <| "Unknown Direction: " ++ somethingElse
        )

{-| JSON decoder for list of Directions -}
decodeDirectionsList : Json.Decode.Decoder (List Direction)
decodeDirectionsList =
  Json.Decode.list decodeDirection
