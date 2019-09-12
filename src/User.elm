module User exposing (..)

import Json.Encode
import Json.Decode

type alias User =
  { username : String
  , color : String
  , score : Int
  , is_admin : Bool
  , is_muted : Bool
  }

  
encodeUser : User -> Json.Encode.Value
encodeUser user =
  Json.Encode.object [ ("username", Json.Encode.string user.username),
                       ("color", Json.Encode.string user.color),
                       ("score", Json.Encode.int user.score),
                       ("is_admin", Json.Encode.bool user.is_admin),
                       ("is_muted", Json.Encode.bool user.is_muted) ]

decodeUser : Json.Decode.Decoder User
decodeUser =
  Json.Decode.map5
    User
    (Json.Decode.field "username" Json.Decode.string)
    (Json.Decode.field "color" Json.Decode.string)
    (Json.Decode.field "score" Json.Decode.int)
    (Json.Decode.field "is_admin" Json.Decode.bool)
    (Json.Decode.field "is_muted" Json.Decode.bool)
  
decodeUsersList : Json.Decode.Decoder (List User)
decodeUsersList =
  Json.Decode.list decodeUser
  