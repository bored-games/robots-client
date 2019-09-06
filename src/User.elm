module User exposing (..)

import Json.Encode
import Json.Decode

type alias User =
  { username : String
  , color : String
  , score : Int
  }

  
encodeUser : User -> Json.Encode.Value
encodeUser user =
  Json.Encode.object [ ("username", Json.Encode.string user.username),
                       ("color", Json.Encode.string user.color),
                       ("score", Json.Encode.int user.score) ]

decodeUser : Json.Decode.Decoder User
decodeUser =
  Json.Decode.map3
    User
    (Json.Decode.field "username" Json.Decode.string)
    (Json.Decode.field "color" Json.Decode.string)
    (Json.Decode.field "score" Json.Decode.int)
  
decodeUsersList : Json.Decode.Decoder (List User)
decodeUsersList =
  Json.Decode.list decodeUser
  