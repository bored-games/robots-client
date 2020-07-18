module Chat exposing (Chatline, decodeChatline, decodeSystemChatline, encodeChatline)

import User exposing (User)
import Json.Encode
import Json.Decode

type alias Chatline =
  { room_name : String
  , user : Maybe User
  , message : String
  , timestamp : String
  }
  
{-| Json encoder for Chatline object -}
encodeChatline : String -> User -> String -> Int -> Json.Encode.Value
encodeChatline room_name user message kind =
  Json.Encode.object [ ("room_name", Json.Encode.string room_name),
                       ("user", User.encodeUser user),
                       ("message", Json.Encode.string message),
                       ("kind", Json.Encode.int kind) ]

{-| Json decoder for Chatline object -}
decodeChatline : Json.Decode.Decoder Chatline
decodeChatline =
  Json.Decode.map4
    Chatline
    (Json.Decode.field "room_name" Json.Decode.string)
    (Json.Decode.maybe (Json.Decode.field "user" User.decodeUser))
    (Json.Decode.field "message" Json.Decode.string)
    (Json.Decode.field "timestamp" Json.Decode.string)

{-| Json decoder for Chatline object -}
decodeSystemChatline : Json.Decode.Decoder Chatline
decodeSystemChatline =
  Json.Decode.map4
    Chatline
    (Json.Decode.field "room_name" Json.Decode.string)
    (Json.Decode.maybe (Json.Decode.field "user" User.decodeUser))
    (Json.Decode.field "message" Json.Decode.string)
    (Json.Decode.field "timestamp" Json.Decode.string)