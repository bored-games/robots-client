module Chat exposing (Chatline, decodeChatline, encodeChatline)

import User exposing (User)
import Json.Encode
import Json.Decode

type alias Chatline =
  { user : User
  , msg : String
  , kind : Int
  }

{-| Json encoder for Chatline object -}
encodeChatline : User -> String -> Int -> Json.Encode.Value
encodeChatline user msg kind =
  Json.Encode.object [ ("user", User.encodeUser user),
                       ("msg", Json.Encode.string msg),
                       ("kind", Json.Encode.int kind) ]

{-| Json decoder for Chatline object -}
decodeChatline : Json.Decode.Decoder Chatline
decodeChatline =
  Json.Decode.map3
    Chatline
    (Json.Decode.field "user" User.decodeUser)
    (Json.Decode.field "msg" Json.Decode.string)
    (Json.Decode.field "kind" Json.Decode.int)

