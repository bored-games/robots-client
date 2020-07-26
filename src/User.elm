module User exposing (User, encodeUser, decodeUser, decodeUsersList)

import Json.Encode
import Json.Decode
import Dict

type alias User =
  { username : String
  , nickname : String
  , color : String
  , team : Int
  , score : Int
  , is_admin : Bool
  , is_muted : Bool
  }

  
encodeUser : User -> Json.Encode.Value
encodeUser user =
  Json.Encode.object [ ("username", Json.Encode.string user.username),
                       ("nickname", Json.Encode.string user.nickname),
                       ("color", Json.Encode.string user.color),
                       ("team", Json.Encode.int user.team),
                       ("score", Json.Encode.int user.score),
                       ("is_admin", Json.Encode.bool user.is_admin),
                       ("is_muted", Json.Encode.bool user.is_muted) ]

decodeUser : Json.Decode.Decoder User
decodeUser =
  Json.Decode.map7
    User
    (Json.Decode.field "username" Json.Decode.string)
    (Json.Decode.field "nickname" Json.Decode.string)
    (Json.Decode.field "color" Json.Decode.string)
    (Json.Decode.field "team" Json.Decode.int)
    (Json.Decode.field "score" Json.Decode.int)
    (Json.Decode.field "is_admin" Json.Decode.bool)
    (Json.Decode.field "is_muted" Json.Decode.bool)
  
decodeUsersList : Json.Decode.Decoder (Dict.Dict String User)
decodeUsersList =
  Json.Decode.map (Dict.map halfUserToUser) (Json.Decode.dict halfUserDecoder)
  
type alias HalfUser =
  { nickname: String
  , color: String
  , team: Int
  , score: Int
  , isadmin: Bool
  , ismuted: Bool
  }

halfUserDecoder : Json.Decode.Decoder HalfUser
halfUserDecoder = 
  Json.Decode.map6 HalfUser
    (Json.Decode.field "nickname" Json.Decode.string)
    (Json.Decode.field "color" Json.Decode.string)
    (Json.Decode.field "team" Json.Decode.int)
    (Json.Decode.field "score" Json.Decode.int)
    (Json.Decode.field "is_admin" Json.Decode.bool)
    (Json.Decode.field "is_muted" Json.Decode.bool)

halfUserToUser : String -> HalfUser -> User
halfUserToUser username {nickname, color, team, score, isadmin, ismuted} =
  User
    username
    nickname
    color
    team
    score
    isadmin
    ismuted

decodeUsersList2 : Json.Decode.Decoder (List User)
decodeUsersList2 =
  Json.Decode.list decodeUser
  