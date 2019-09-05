module Grid exposing (..)

import Array exposing (Array)
import Maybe
import Tuple

type alias Grid a =
  Array ( Array a)

-- (x, y)
type alias Position =
  ( Int, Int )

toPosition : Int -> Int -> Position
toPosition x y =
  (x, y)

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


type alias Filler a =
  Int -> Int -> a


{-| Create a grid `width` units by `height` units, filling each cell according
to the cell's Position.
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


{-| Create a grid just like [`Grid#rectangle`](Grid#rectangle), except just
copy a value into each cell.
    get (2, 1) (rectangle 4 2 "foo") == Just "foo"
-}
repeat : Int -> Int -> a -> Grid a
repeat x y occupant =
  rectangle x y (always << always occupant)


{-| Like `repeat`, except make a square grid.
-}
repeatSquare : Int -> a -> Grid a
repeatSquare size occupant =
  square size (always << always occupant)



{-| Fetch the column index from a `Position`. Useful with `column`
-}
toColumn : Position -> Int
toColumn =
  Tuple.first


{-| Fetch the row index from a `Position`. Useful with `row`.
-}
toRow : Position -> Int
toRow =
  Tuple.second



-- TODO: Export or delete
-- width : Grid a -> Int
-- width grid =
--     Array.get 0 grid
--         |> Maybe.map Array.length
--         |> Maybe.withDefault 0
-- height : Grid a -> Int
-- height grid =
--     Array.length grid


{-| Fetch the occupant at a given `Position`.
    get (2,4) (square 6 (*)) == Just 8
-}
get : Position -> Grid a -> Maybe a
get coord grid =
  let
    a1 = Array.get (toRow coord) grid
  in
    case a1 of
      Nothing ->
        Nothing
    
      Just val ->
        Array.get (toColumn coord) val


{-| Overwrite the occupant at a given `Position` -}
set : Position -> a -> Grid a -> Grid a
set coord occupant grid =
  row (toRow coord) grid
    |> Maybe.map (Array.set (toColumn coord) occupant)
    |> Maybe.map (\r -> Array.set (toRow coord) r grid)
    |> Maybe.withDefault grid
    |> Debug.log "new"


map : (a -> b) -> Grid a -> Grid b
map f grid =
  Array.map (Array.map f) grid


mapWithPosition : (Position -> a -> b) -> Grid a -> Grid b
mapWithPosition f grid =
  Array.indexedMap
    (\y -> Array.indexedMap (\x -> f (toPosition x y)))
    grid