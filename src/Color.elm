module Color exposing (..)

import Json.Decode

type Color
  = Red
  | Green
  | Blue
  | Yellow
  | Silver

{-| Return Color from lowercase string -}
fromString : String -> Maybe Color
fromString str =
  case str of
    "red" -> Just Red
    "green" -> Just Green
    "blue" -> Just Blue
    "yellow" -> Just Yellow
    "silver" -> Just Silver
    _ -> Nothing

{-| Return string of corresponding Color -}
toString : Maybe Color -> String
toString color =
  case color of
    Nothing -> "unknown-color"
    Just Red -> "red"
    Just Green -> "green"
    Just Blue -> "blue"
    Just Yellow -> "yellow"
    Just Silver -> "silver"

{-| JSON decoder for Color -}
decodeColorSymbol : Json.Decode.Decoder Color
decodeColorSymbol =
    Json.Decode.string
        |> Json.Decode.andThen (\str ->
           case str of
                "red"         -> Json.Decode.succeed Red
                "green"       -> Json.Decode.succeed Green
                "blue"        -> Json.Decode.succeed Blue
                "yellow"      -> Json.Decode.succeed Yellow
                "silver"      -> Json.Decode.succeed Silver
                somethingElse -> Json.Decode.fail <| "Unknown Color: " ++ somethingElse
        )