module Coordinate exposing (..)

import Tuple
import String 

type alias Coordinate =
  ( Int, Int )

{-| Construct `Coordinate` from (x, y) integer pair. -}
toCoordinate : Int -> Int -> Coordinate
toCoordinate x y =
  (x, y)

{-| Get string for `Coordinate` as "(x, y)". -}
toString : Coordinate -> String
toString coord =
  "(" ++ (Tuple.first coord |> String.fromInt) ++ ", " ++ (Tuple.second coord |> String.fromInt) ++ ")"
  
{-| Fetch the column index from a `Coordinate`. -}
toColumn : Coordinate -> Int
toColumn =
  Tuple.first


{-| Fetch the row index from a `Coordinate`. -}
toRow : Coordinate -> Int
toRow =
  Tuple.second