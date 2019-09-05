port module Main exposing (Model, Msg(..), init, inputPort, main, outputPort, subscriptions, update, view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (style, type_, placeholder, value, class)
import Html.Events exposing (onInput, onSubmit, onClick)
import Time
import Tuple
import Set
import Json.Encode
import Json.Decode
import Grid

-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias JSONMessage = 
  { code : Int 
  , content : Json.Encode.Value
  }

type alias Position = 
  { x : Int 
  , y : Int
  }

type alias Goal =
  { pos : Position
  , symbol : GoalSymbol
  , active : Bool
  }

type alias Robot =
  { pos : Position
  , color : Color
  , moves : List Direction
  }

type alias Move =
  { color : Color
  , direction : Direction
  }

type alias User =
  { username : String
  , color : String
  , score : Int
  }

type alias Chatline =
  { user : User
  , msg : String
  , kind : Int
  }

type alias Model =
  { debugString : String
  , keys : Keys
  , user : User
  , users : List User
  , chat : List Chatline
  , messageInProgress : String
  , nameInProgress : String
  , colorInProgress : String
  , boundaryBoard : Grid.Grid Color
  , testboard : List ( List Int )
  , goal : GoalSymbol
  , goalList : List Goal
  , toggleStates : { settings: String, pollOptions: String, emoticons: String, countdown: String }
  , countdown : Int
  , currentTimer : Int
  , robots : List Robot
  , activeRobot : Maybe Robot
 -- , legalMoves : List ( Move )
  , movesQueue : List ( Move )
  }

type alias Keys =
  { up : Bool
  , left : Bool
  , down : Bool
  , right : Bool
  , space : Bool
  , enter : Bool
  , one : Bool
  , two : Bool
  , three : Bool
  , four : Bool
  , five : Bool
  , esc : Bool
  }

systemUser = { username = "System", color = "#ccc", score = 0 }

testFill : Int -> Int -> Color
testFill x y =
  Blue

init : () -> (Model, Cmd Msg)
init _ =
  (Model
    "Initialized model."
    (Keys False False False False False False False False False False False False)
    { username = "patty", color = "#6c6adc", score = 0 }
    [ ] -- `users` (and scores)
    [ ] -- `chat`
    "" -- `messageInProgress`
    "" -- `nameInProgress`
    "" -- `colorInProgress`
    (Grid.square 16 (testFill) )                    -- boundaryBoard
    [ [ 7,  8,  7,  2, 15, 17,  2,  2,  8,  7, 16, 15,  2,  2,  2,  8],
      [ 5,  1,  1,  1,  8,  5,  1,  1,  1,  1,  3,  7,  1,  1,  1,  9],
      [ 5,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  4, 26],
      [14,  6,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  8, 14],
      [22,  2,  1,  1, 13,  4,  1,  1,  1,  9,  5,  1,  1,  1,  1,  3],
      [ 6,  1,  1,  1,  3,  7,  1,  1,  1,  2, 11,  1,  1,  1,  1,  3],
      [ 7,  1,  1,  9,  5,  1, 13,  4,  4, 10,  1,  3,  6,  1,  1,  3],
      [ 5,  1,  4, 17, 11,  1,  3,  0,  0,  5,  1, 12,  2,  1,  1,  3],
      [ 5,  1,  8,  5,  1,  1,  3,  0,  0,  5,  1,  1,  1,  1,  1,  3],
      [ 5,  1,  1,  1,  3,  6, 12,  2,  2, 11, 13,  4,  1,  1,  1,  3],
      [ 5,  9,  5,  1, 12,  2,  1,  1,  1,  1,  3,  7,  1,  1,  1,  3],
      [ 5,  2, 11,  1,  4, 10,  1,  1,  1,  1,  4, 10,  3,  6,  1,  9],
      [ 6,  1,  1,  1,  8,  5,  1,  1,  1,  1,  8,  5, 12,  2,  1,  8],
      [ 7,  1,  1,  1,  1, 13,  4,  1,  1,  1,  1,  1,  9,  5,  1,  3],
      [ 5,  1,  1,  1,  1,  3,  7,  1,  1,  1,  1,  1,  2, 11,  1,  3],
      [ 6,  4,  4,  9,  6,  4,  4,  4,  4,  9,  6,  4,  4,  4,  4,  9] ]      -- board
    RedMoon                                                                   -- goalSymbol
    [ { symbol = RedMoon, pos = { x = 1, y = 3 }, active = True }, { symbol = BlueGear, pos = { x = 14, y = 3 }, active = True } ]
    { settings = "none",
      pollOptions = "none",
      emoticons = "none",
      countdown = "flex" }                                                   -- toggleStates
    60                                                                       -- countdown
    0                                                                        -- currentTimer
    []                                                                       -- robots
    Nothing                                                                  -- activeRobot
  --  [ (Move Red Up), (Move Yellow Right)]                                    -- legalMoves
    []                                                                       -- movesQueue
  ,  outputPort (Json.Encode.encode 0 (Json.Encode.object [ ("code", Json.Encode.int 200), ("content", encodeUser { username = "patty", color = "#6c6adc", score = 0 }) ] ) ) -- initialize user?
  )

type Color
  = Red
  | Green
  | Blue
  | Yellow
  | Silver

-- TODO: Include Wildcard?
type GoalSymbol
  = RedMoon
  | GreenMoon
  | BlueMoon
  | YellowMoon
  | RedPlanet
  | GreenPlanet
  | BluePlanet
  | YellowPlanet
  | GreenCross
  | RedCross
  | BlueCross
  | YellowCross
  | RedGear
  | GreenGear
  | BlueGear
  | YellowGear

type Direction
  = Left
  | Up
  | Down
  | Right

-- UPDATE


type Msg
  = SetName String
  | SetColor String
  | UpdateSettings
  | SetMessage String
  | SendMessage
  | NewGame
  | NewGoal GoalSymbol
  | IncrementScore User
  | TogglePollOptions
  | ToggleSettings
  | ToggleEmoticons
  | InsertEmoticon String
  | DisplayCountdown String
  | Tick Time.Posix
  | GetJSON Json.Encode.Value      -- Parse incoming JSON
  | GetRobotList Json.Encode.Value -- 101
  | GetGoalList Json.Encode.Value  -- 102
  | GetUsersList Json.Encode.Value -- 200
  | GetUser Json.Encode.Value      -- 201
  | GetChat Json.Encode.Value      -- 202
  | KeyChanged Bool String
  | SetActiveRobot Color
  | AddMove Direction
  | PopMove
  | ClearMoves

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "MESSAGE: " msg of
    SetName name ->
      ( { model | nameInProgress = name }, Cmd.none )
      
    SetColor color ->
      ( { model | colorInProgress = color }, Cmd.none )
      
    UpdateSettings ->
      let
        oldUser = model.user
        oldUsers = model.users
        oldName = oldUser.username
        oldToggleStates = model.toggleStates
        newColor = model.colorInProgress
        newName = if (List.member model.nameInProgress (List.map .username oldUsers)) then oldName else (if String.length model.nameInProgress > 0 then model.nameInProgress else oldName)
        newUser = { oldUser | username = newName, color = newColor }
        replaceUser testUser =
          if (oldUser.username == testUser.username) then
            { testUser | username = newName, color = newColor }
          else
            testUser
        newUsers = List.map replaceUser oldUsers
        newToggleStates = { oldToggleStates | settings = "none" }
      in
      ( { model
       | user = newUser
       , users = newUsers
       , nameInProgress = ""
       , toggleStates = newToggleStates
      }
      , outputPort (Json.Encode.encode 0 (Json.Encode.object [ ("code", Json.Encode.int 201), ("content", encodeUser newUser) ] ) ) )

    SetMessage message ->
      ( { model | messageInProgress = message }
      , Cmd.none
      )

    SendMessage ->
      let
        newmsg = String.trim model.messageInProgress
      in
        if newmsg == "" then
          ( { model | messageInProgress = "" }, Cmd.none )
        else
          ( { model | messageInProgress = "" }
          , outputPort (Json.Encode.encode
                          0
                        ( Json.Encode.object
                        [ ( "code", Json.Encode.int 202),
                          ( "content", encodeChatline model.user newmsg 0 ) ] ) ) )

    NewGame -> -- TODO!
      ( { model
        | currentTimer = 0
        , countdown = 0
        }
        , outputPort (Json.Encode.encode
                        0
                      ( Json.Encode.object
                      [ ( "code", Json.Encode.int 100),
                        ( "content", Json.Encode.string "" ) ] ) ) )

    NewGoal newGoal ->
      ( { model | goal = newGoal }, Cmd.none )

    IncrementScore user ->
      let
        incrementScore testUser =
          if (user.username == testUser.username) then
            { testUser | score = (testUser.score + 1) }
          else
            testUser
        users = List.map incrementScore model.users
      in
        ( { model | users = users }
        , Cmd.none
        )

    TogglePollOptions ->
      let
        oldToggleStates = model.toggleStates
        newToggleStates =
          { oldToggleStates
          | pollOptions = (if oldToggleStates.pollOptions == "none" then "flex" else "none")
          , settings = "none"
          , emoticons = "none" }
      in
        ( { model | toggleStates = newToggleStates }
        , Cmd.none
        )
      
    ToggleSettings ->
      let
        oldToggleStates = model.toggleStates
        newToggleStates =
          { oldToggleStates
            | settings = (if oldToggleStates.settings == "none" then "flex" else "none")
            , pollOptions = "none"
            , emoticons = "none" }
      in
        ( { model | toggleStates = newToggleStates }, Cmd.none )
      
    ToggleEmoticons ->
      let
        oldToggleStates = model.toggleStates
        newToggleStates =
          { oldToggleStates
          | emoticons = (if oldToggleStates.emoticons == "none" then "flex" else "none")
          , pollOptions = "none"
          , settings = "none" }
      in
        ( { model | toggleStates = newToggleStates }, Cmd.none )
    
    InsertEmoticon str ->
      ( { model | messageInProgress = (model.messageInProgress ++ " :" ++ str ++ ": ") }, Cmd.none )

    DisplayCountdown status ->
      let
          oldToggleStates = model.toggleStates
          newToggleStates = { oldToggleStates | countdown = status }
      in
        ( { model | toggleStates = newToggleStates }, Cmd.none )

    Tick newTime ->
      ( { model | currentTimer = (model.currentTimer + 1) }, Cmd.none )

    GetJSON json ->
      case (Json.Decode.decodeValue decodeJSON json) of
        Ok {code, content} ->
          case code of
            100 ->
              ((Debug.log "TODO: Update board layout" model), Cmd.none )
            101 ->
              update (GetRobotList content) model
            102 ->
              update (GetGoalList content) model
            200 ->
              update (GetUsersList content) model
            201 ->
              update (GetUser content) model
            202 ->
              update (GetChat content) model
            _ ->
              ((Debug.log "Error: missing code in JSON message" model), Cmd.none ) -- Error: missing code

        Err _ ->
          ( { model | users = []}, Cmd.none )

    GetRobotList json ->
      case (Json.Decode.decodeValue decodeRobotsList json) of
        Ok robotList ->
          ( { model | robots = robotList}, Cmd.none )
        Err _ ->
          ( { model | robots = []}, Cmd.none )
          
    GetGoalList json ->
      case (Json.Decode.decodeValue decodeGoalList json) of
        Ok goalList ->
          let
            activeGoal =
              case List.head (List.filter .active goalList) of
              Nothing ->
                RedMoon -- TODO: handle error?
              Just anyGoal ->
                .symbol anyGoal
          in
            ( { model
                | goalList = goalList
                , goal = activeGoal
              }, Cmd.none )
        Err _ ->
          ( { model | goalList = []}, Cmd.none )

    GetUsersList json ->
      case (Json.Decode.decodeValue decodeUsersList json) of
        Ok usersList ->
          ( { model | users = usersList}, Cmd.none )
        Err _ ->
          ( { model | users = []}, Cmd.none )

    GetUser json ->
      case (Json.Decode.decodeValue decodeUser json) of
        Ok user ->
          ( { model | user = user}, Cmd.none )
        Err _ ->
          ( { model | users = []}, Cmd.none )

    GetChat json ->
      case (Json.Decode.decodeValue decodeChatList json) of
        Ok chatList ->
          ( { model | chat = chatList}, Cmd.none )
        Err _ ->
          ( { model | chat = [] }, Cmd.none )

    KeyChanged isDown key ->
      let
        newKeys = updateKeys isDown key model.keys
        debugStr = (if .up newKeys then "↑" else "_")
                ++ (if .down newKeys then "↓" else "_")
                ++ (if .left newKeys then "←" else "_")
                ++ (if .right newKeys then "→" else "_")
                ++ (if .space newKeys then " " else "_")
                ++ (if .enter newKeys then "=" else "_")
                ++ (if .one newKeys then "1" else "_")
                ++ (if .two newKeys then "2" else "_")
                ++ (if .three newKeys then "3" else "_")
                ++ (if .four newKeys then "4" else "_")
                ++ (if .five newKeys then "5" else "_")
                ++ (if .one newKeys then "r" else "_")
                ++ (if .two newKeys then "g" else "_")
                ++ (if .three newKeys then "b" else "_")
                ++ (if .four newKeys then "y" else "_")
                ++ (if .five newKeys then "s" else "_")
                ++ (if .esc newKeys then "e" else "_")
        activeRobot =
          if isDown then
            case key of
              "1"      -> (getRobotByColor Red model.robots)
              "r"      -> (getRobotByColor Red model.robots)
              "2"      -> (getRobotByColor Green model.robots)
              "g"      -> (getRobotByColor Green model.robots)
              "3"      -> (getRobotByColor Blue model.robots)
              "b"      -> (getRobotByColor Blue model.robots)
              "4"      -> (getRobotByColor Yellow model.robots)
              "y"      -> (getRobotByColor Yellow model.robots)
              "5"      -> (getRobotByColor Silver model.robots)
              "s"      -> (getRobotByColor Silver model.robots)
              "Escape" -> Nothing
              _        -> model.activeRobot
          else
            model.activeRobot

        newQueue =
          if isDown then
            case key of
              "ArrowLeft"  -> pushMove Left model.activeRobot model.movesQueue
              "ArrowRight" -> pushMove Right model.activeRobot model.movesQueue
              "ArrowUp"    -> pushMove Up model.activeRobot model.movesQueue
              "ArrowDown"  -> pushMove Down model.activeRobot model.movesQueue
              "Escape"     -> []
              _ -> model.movesQueue
          else
            model.movesQueue
      in
        ( { model | keys = newKeys, debugString = debugStr, activeRobot = activeRobot, movesQueue = newQueue }, Cmd.none )

    -- Set active robot color for next move
    SetActiveRobot color ->
      ( { model | activeRobot = getRobotByColor color model.robots}, Cmd.none )
    
    -- Only push if there is an active robot and move is in set of legal moves.
    AddMove dir ->
      let
        newQueue = pushMove dir model.activeRobot model.movesQueue
      in
        ( { model | movesQueue = newQueue }, Cmd.none )
    
    -- Remove last move from queue (for Undo)
    PopMove ->
      let
        moves =
          case model.movesQueue of
            [] -> []
            a::b -> b
      in
      ( { model | movesQueue = moves }, Cmd.none )
      
    -- Remove all moves from queue and reset the active robot color
    ClearMoves ->
      ( { model | movesQueue = [], activeRobot = Nothing}, Cmd.none )


-- TODO: Move to elixir
-- Get all legal moves for 1 robot
getBoardLegalMoves : Robot -> List ( List ( Int ) ) -> List Move
getBoardLegalMoves robot board =
  []

getRobotByColor : Color -> List Robot -> Maybe Robot
getRobotByColor color robots =
  let
    matchRobotByColor robot =
      robot.color == color
  in  
    (List.head (List.filter matchRobotByColor robots))

getColorFromRobot : Maybe Robot -> Maybe Color
getColorFromRobot robot =
  case robot of
    Nothing -> Nothing
    Just r -> Just (.color r)


pushMove : Direction -> Maybe Robot -> List Move -> List Move
pushMove dir activeRobot oldQueue =
  case activeRobot of
    Nothing -> oldQueue
    Just robot ->
      if (List.member dir robot.moves) then
        (Move robot.color dir) :: oldQueue
      else
        oldQueue

-- debug function:
printMoveList : List Move -> String
printMoveList moveList =
  case moveList of
    a::b ->
      (getColorString (Just (.color a))) ++ ":" ++ getDirectionString (.direction a) ++ " -> " ++ (printMoveList b)
    _ ->
      ""


updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
  case key of
    " "          -> { keys | space = isDown }
    "ArrowUp"    -> { keys | up    = isDown }
    "ArrowLeft"  -> { keys | left  = isDown }
    "ArrowDown"  -> { keys | down  = isDown }
    "ArrowRight" -> { keys | right = isDown }
    "Enter"      -> { keys | enter = isDown }
    "1"          -> { keys | one   = isDown }
    "2"          -> { keys | two   = isDown }
    "3"          -> { keys | three = isDown }
    "4"          -> { keys | four  = isDown }
    "5"          -> { keys | five  = isDown }
    "r"          -> { keys | one   = isDown }
    "g"          -> { keys | two   = isDown }
    "b"          -> { keys | three = isDown }
    "y"          -> { keys | four  = isDown }
    "s"          -> { keys | five  = isDown }
    "Escape"     -> { keys | esc   = isDown }
    _            -> keys

encodeUser : User -> Json.Encode.Value
encodeUser user =
  Json.Encode.object [ ("username", Json.Encode.string user.username),
                       ("color", Json.Encode.string user.color),
                       ("score", Json.Encode.int user.score) ]

encodeChatline : User -> String -> Int -> Json.Encode.Value
encodeChatline user msg kind =
  Json.Encode.object [ ("user", encodeUser user),
                       ("msg", Json.Encode.string msg),
                       ("kind", Json.Encode.int kind) ]

decodeGoalSymbol : Json.Decode.Decoder GoalSymbol
decodeGoalSymbol =
  Json.Decode.string
    |> Json.Decode.andThen (\str ->
      case str of
        "RedMoon"      -> Json.Decode.succeed RedMoon
        "GreenMoon"    -> Json.Decode.succeed GreenMoon
        "BlueMoon"     -> Json.Decode.succeed BlueMoon
        "YellowMoon"   -> Json.Decode.succeed YellowMoon
        "RedPlanet"    -> Json.Decode.succeed RedPlanet
        "GreenPlanet"  -> Json.Decode.succeed GreenPlanet
        "BluePlanet"   -> Json.Decode.succeed BluePlanet
        "YellowPlanet" -> Json.Decode.succeed YellowPlanet
        "GreenCross"   -> Json.Decode.succeed GreenCross
        "RedCross"     -> Json.Decode.succeed RedCross
        "BlueCross"    -> Json.Decode.succeed BlueCross
        "YellowCross"  -> Json.Decode.succeed YellowCross
        "RedGear"      -> Json.Decode.succeed RedGear
        "GreenGear"    -> Json.Decode.succeed GreenGear
        "BlueGear"     -> Json.Decode.succeed BlueGear
        "YellowGear"   -> Json.Decode.succeed YellowGear
        somethingElse  -> Json.Decode.fail <| "Unknown Goal Symbol: " ++ somethingElse
    )

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

decodeJSON : Json.Decode.Decoder JSONMessage
decodeJSON =
  Json.Decode.map2
    JSONMessage
    (Json.Decode.field "code" Json.Decode.int)
    (Json.Decode.field "content" Json.Decode.value)

decodeRobot : Json.Decode.Decoder Robot
decodeRobot =
  Json.Decode.map3
    Robot
    (Json.Decode.field "pos" (Json.Decode.map2 Position
      (Json.Decode.field "x" Json.Decode.int)
      (Json.Decode.field "y" Json.Decode.int)
    ))
    (Json.Decode.field "color" decodeColorSymbol)
    (Json.Decode.field "moves" decodeDirectionsList)
  
decodeRobotsList : Json.Decode.Decoder (List Robot)
decodeRobotsList =
  Json.Decode.list decodeRobot

decodeDirectionsList : Json.Decode.Decoder (List Direction)
decodeDirectionsList =
  Json.Decode.list decodeDirection

decodeGoal : Json.Decode.Decoder Goal
decodeGoal =
  Json.Decode.map3
    Goal
    (Json.Decode.field "pos" (Json.Decode.map2 Position
      (Json.Decode.field "x" Json.Decode.int)
      (Json.Decode.field "y" Json.Decode.int)
    ))
    (Json.Decode.field "symbol" decodeGoalSymbol)
    (Json.Decode.field "active" Json.Decode.bool)
  
decodeGoalList : Json.Decode.Decoder (List Goal)
decodeGoalList =
  Json.Decode.list decodeGoal
  

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
  

decodeChatline : Json.Decode.Decoder Chatline
decodeChatline =
  Json.Decode.map3
    Chatline
    (Json.Decode.field "user" decodeUser)
    (Json.Decode.field "msg" Json.Decode.string)
    (Json.Decode.field "kind" Json.Decode.int)

decodeChatList : Json.Decode.Decoder (List Chatline)
decodeChatList =
  Json.Decode.list decodeChatline


-- SUBSCRIPTIONS

port outputPort : (String) -> Cmd msg
port inputPort : (Json.Encode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 1000 Tick
    , inputPort GetJSON
    , Browser.Events.onKeyUp (Json.Decode.map (KeyChanged False) (Json.Decode.field "key" Json.Decode.string))
    , Browser.Events.onKeyDown (Json.Decode.map (KeyChanged True) (Json.Decode.field "key" Json.Decode.string))
    ]



-- VIEW

goalToString : GoalSymbol -> { plaintext: String, filename: String }
goalToString goal =
  case goal of
    RedMoon -> { plaintext = "Red Moon", filename = "red-moon" }
    GreenMoon -> { plaintext = "Green Moon", filename = "green-moon" }
    BlueMoon -> { plaintext = "Blue Moon", filename = "blue-moon" }
    YellowMoon -> { plaintext = "Yellow Moon", filename = "yellow-moon" }
    RedPlanet -> { plaintext = "Red Planet", filename = "red-planet" }
    GreenPlanet -> { plaintext = "Green Planet", filename = "green-planet" }
    BluePlanet -> { plaintext = "Blue Planet", filename = "blue-planet" }
    YellowPlanet -> { plaintext = "Yellow Planet", filename = "yellow-planet" }
    GreenCross -> { plaintext = "Green Cross", filename = "red-cross" }
    RedCross -> { plaintext = "Red Cross", filename = "green-cross" }
    BlueCross -> { plaintext = "Blue Cross", filename = "blue-cross" }
    YellowCross -> { plaintext = "Yellow Cross", filename = "yellow-cross" }
    RedGear -> { plaintext = "Red Gear", filename = "red-gear" }
    GreenGear -> { plaintext = "Green Gear", filename = "green-gear" }
    BlueGear -> { plaintext = "Blue Gear", filename = "blue-gear" }
    YellowGear -> { plaintext = "Yellow Gear", filename = "yellow-gear" }


formatTimer : Int -> String
formatTimer seconds =
  let
    sec = (modBy 60 seconds)
    hrs = ((seconds // 60) // 60)
    min = (seconds // 60) - (hrs * 60)
  in
    (if hrs > 0 then (String.fromInt hrs ++ ":" ) else "") ++ String.fromInt min ++ ":" ++ (if sec < 10 then "0" else "") ++ (String.fromInt sec)

drawScore : User -> Html Msg
drawScore user =
  div [ class "score" ] 
  [ div [ class "score__username", style "color" user.color ] [ text user.username ]
  , text (String.fromInt user.score)
  ]

drawMessage : Chatline -> Html Msg
drawMessage message =
  case message.kind of
    0 -> -- regular chat message
      ( div [ class "chat__line" ] 
        ( div [ class "chat__username", style "color" message.user.color ]
          [ text message.user.username ] :: parseEmoticonHtml message.msg )
      )
    _ -> -- system message
      (div [ class "chat__line" ] [ em [ class "chat__line--system" ] [ text message.msg ] ])


drawAll : Int -> (List ( List Int )) -> List Robot -> List Goal -> List ( List (Html Msg) )
drawAll rowi board robots goals =
  case board of
    [] -> []
    
    a::b ->
      drawRow rowi 0 a robots goals :: drawAll (rowi + 1) b robots goals

drawRow : Int -> Int -> (List Int) -> List Robot -> List Goal -> List (Html Msg)
drawRow rowi colj rows robots goals =
  case rows of
    [] -> []
  
    a::b ->
      drawSquare rowi colj a robots goals :: drawRow rowi (colj + 1) b robots goals

drawSquare : Int -> Int -> Int -> List Robot -> List Goal -> Html Msg
drawSquare rowi colj val robots goals =
  let
    robotSquares = List.map .pos robots
    matchedRobot = 
      case (List.head (List.filter (matchRobot rowi colj) robots)) of
        Nothing ->
          ""
        Just matchedRobotObj ->
          (getColorString (Just (.color matchedRobotObj)))

    matchedGoal = 
      case (List.head (List.filter (matchGoal rowi colj) goals)) of
        Nothing ->
          ""
        Just matchedGoalObj ->
          (.filename (goalToString matchedGoalObj.symbol))

  in
    case val of
      0 ->
        div [ class "square square--block" ]
         [
           div [ class ("goal "++matchedGoal) ] []
         , div [ class ("robot robot--"++matchedRobot) ] []
        ]
      n -> 
        div [ class ("square square--" ++ String.fromInt n) ]
         [
           div [ class ("goal "++matchedGoal) ] []
         , div [ class ("robot robot--"++matchedRobot) ] []
         ]

getColorSymbol : String -> Maybe Color
getColorSymbol str =
  case str of
    "red" -> Just Red
    "green" -> Just Green
    "blue" -> Just Blue
    "yellow" -> Just Yellow
    "silver" -> Just Silver
    _ -> Nothing

getColorString : Maybe Color -> String
getColorString color =
  case color of
    Nothing -> "unknown-color"
    Just Red -> "red"
    Just Green -> "green"
    Just Blue -> "blue"
    Just Yellow -> "yellow"
    Just Silver -> "silver"

getDirectionString : Direction -> String
getDirectionString dir =
  case dir of
    Left -> "left"
    Up -> "up"
    Down -> "down"
    Right -> "right"

matchRobot : Int -> Int -> Robot -> Bool
matchRobot rowi colj robot =
  robot.pos == { x = colj, y = rowi }

matchGoal : Int -> Int -> Goal -> Bool
matchGoal rowi colj record =
  record.pos == { x = colj, y = rowi }

drawEmoticon : String -> Html Msg
drawEmoticon str =
  div [ class ("emoticon emoticon--" ++ str), onClick (InsertEmoticon str) ] []

emoticonList = [ "cool", "crazy", "damn", "geek", "grin", "huh", "lol", "love", "omg", "pout", "sad", "smile", "stars", "ugh", "waiting", "whoopsy", "wink", "wtf" ]
drawEmoticons : List (Html Msg)
drawEmoticons =
  List.map drawEmoticon emoticonList

drawSettings : Model -> List (Html Msg)
drawSettings model =
  [ h2 [ ] [ text "Settings" ]
  , input [ type_ "text", onInput SetName, placeholder ("Set name (" ++ model.user.username ++ ")"), value model.nameInProgress ] []
  , select [ onInput SetColor ]
    [
      option [ value "", style "color" "#707070" ] [ text "Change color" ]
    , option [ value "#e05e5e", style "color" "#e05e5e" ] [ text "red" ]
    , option [ value "#e09f5e", style "color" "#e09f5e" ] [ text "orange" ]
    , option [ value "#e0e05e", style "color" "#e0e05e" ] [ text "yellow" ]
    , option [ value "#9fe05e", style "color" "#9fe05e" ] [ text "lime" ]
    , option [ value "#5ee05e", style "color" "#5ee05e" ] [ text "dark sea" ]
    , option [ value "#5ee09f", style "color" "#5ee09f" ] [ text "aquamarine" ]
    , option [ value "#5ee0e0", style "color" "#5ee0e0" ] [ text "azure" ]
    , option [ value "#5e9fe0", style "color" "#5e9fe0" ] [ text "cornflower" ]
    , option [ value "#5e5ee0", style "color" "#5e5ee0" ] [ text "periwinkle" ]
    , option [ value "#9f5ee0", style "color" "#9f5ee0" ] [ text "dendrobium " ]
    , option [ value "#e05ee0", style "color" "#e05ee0" ] [ text "french rose" ]
    , option [ value "#e05e9f", style "color" "#e05e9f" ] [ text "barbie-mobile" ]
    , option [ value "#b19278", style "color" "#b19278" ] [ text "english elm" ]
    , option [ value "#e0e0e0", style "color" "#e0e0e0" ] [ text "gainsboro" ]
  ]
  , input [ type_ "submit", class "submit", value "Update", onClick UpdateSettings ] []
  ]
  
drawPollOptions : List (Html Msg)
drawPollOptions =
  [ h2 [ ] [ text "Poll Commands" ]
  , div [ class "poll__command" ] [ text ( "Coming soon." ) ]
  , div [ class "poll__command" ] [ text ( "/poll new" ) ]
  ]

parseEmoticonHtml : String -> List (Html Msg)
parseEmoticonHtml str =
  let
    parseEmoticon ind1 ind2 teststr =
      let
        parsedStr = String.slice (ind1+1) ind2 teststr
      in
        case List.member parsedStr emoticonList of
          True ->
            span [ class ("emoticon emoticon--" ++ parsedStr) ] [] :: parseEmoticonHtml (String.dropLeft (ind2+1) str)
          False ->
            text (":"++parsedStr) :: parseEmoticonHtml (String.dropLeft (ind2) str)
  in
    case (String.indexes ":" str) of
      a::b::rest ->
        text (String.slice 0 a str)
        :: parseEmoticon a b str

      _ ->
        [ text str ]

onEnter : msg -> Attribute msg
onEnter msg =
  let
    filterKey code =
      if code == 13 then -- Enter was pressed
        Json.Decode.succeed { message = msg, stopPropagation = False, preventDefault = True }
      else
        Json.Decode.fail "ignored input"
    decoder =
      Html.Events.keyCode |> Json.Decode.andThen filterKey
  
  in 
    Html.Events.custom "keydown" decoder

view : Model -> Html Msg
view model =
  let
    drawChat chat =
      (List.reverse chat) |> List.map drawMessage
    drawScores users =
      users |> List.map drawScore
    drawBoard board =
      List.concat ( drawAll 0 board model.robots model.goalList )

  in
    div [ class "container" ]
      [
        div [ class "scorebar" ]
        [
          div [ class "scores" ] 
            (h2 [] [ text "Scoreboard" ] :: ((List.reverse (List.sortBy .score model.users)) |> drawScores))
        , div [ class "timer", onClick (IncrementScore model.user) ]
          [
            div [ class ("goal " ++ .filename (goalToString model.goal)) ] [ ]
          , div [ class "timer__countdown" ]
            [
              span [] [ text (formatTimer model.countdown) ]
            , div [ class "icon icon--timer" ] []
            ]
          , div [ class "timer__current-timer" ]
            [ 
              span [] [ text (formatTimer model.currentTimer) ]
            , div [ class "icon icon--clock" ] []
            ]
          ]
        ]
      , div [ class "main"] [
          div [ class "controls" ]
          [ div [ class "controls__robots" ]
            [ div [ class ("controls__robot controls__red" ++ (if (getColorFromRobot model.activeRobot == Just Red) then " active" else "")), onClick (SetActiveRobot Red)] []
            , div [ class ("controls__robot controls__green" ++ (if (getColorFromRobot model.activeRobot == Just Green) then " active" else "")), onClick (SetActiveRobot Green) ] []
            , div [ class ("controls__robot controls__blue" ++ (if (getColorFromRobot model.activeRobot == Just Blue) then " active" else "")), onClick (SetActiveRobot Blue) ] []
            , div [ class ("controls__robot controls__yellow" ++ (if (getColorFromRobot model.activeRobot == Just Yellow) then " active" else "")), onClick (SetActiveRobot Yellow) ] []
            , div [ class ("controls__robot controls__silver" ++ (if (getColorFromRobot model.activeRobot == Just Silver) then " active" else "")), onClick (SetActiveRobot Silver) ] []
            ]
          , div [ class "controls__directions" ]
            [ div [ class ("controls__button controls__left" ++ (if model.keys.left then " active" else "")), onClick (AddMove Left) ] []
            , div [ class ("controls__button controls__up" ++ (if model.keys.up then " active" else "")), onClick (AddMove Up) ] []
            , div [ class ("controls__button controls__right" ++ (if model.keys.right then " active" else "")), onClick (AddMove Right) ] []
            , div [ class ("controls__button controls__down" ++ (if model.keys.down then " active" else "")), onClick (AddMove Down) ] []
            , div [ class ("controls__button controls__undo" ++ (if List.isEmpty model.movesQueue then " inactive" else "")), onClick PopMove ] []
            , div [ class ("controls__button controls__cancel" ++ (if List.isEmpty model.movesQueue then " inactive" else "")), onClick ClearMoves ] []
            ]
          , div [ ] [ text ( getColorString (Grid.get (1, 1) model.boundaryBoard) ++ "   " ++ model.debugString ++ "   " ++ (printMoveList (List.reverse model.movesQueue))) ]
           ]
        , div [ class "game" ] (model.testboard |> drawBoard)
        ]
      , div [ class "sidebar" ]
        [
          div [ class "chat" ]
            (h2 [] [ text "Chat" ] :: (List.reverse model.chat |> drawChat))
        , div [ class "sidebar__settings", style "display" model.toggleStates.settings ] (drawSettings model)
        , div [ class "sidebar__polloptions", style "display" model.toggleStates.pollOptions ] (drawPollOptions)
        , div [ class "message"]
          [ textarea [ class "message__box", onInput SetMessage, onEnter SendMessage, placeholder "Send a message", value model.messageInProgress, Html.Attributes.maxlength 255 ] []
          , div [ class "sidebar__emoticons", style "display" model.toggleStates.emoticons ] (drawEmoticons)
          , div [ class "message__actions" ]
            [
            button [ class "settings", onClick ToggleSettings ] []
          , button [ class "poll", onClick TogglePollOptions ] []
          , button [ class "new", onClick NewGame ] []
          , div [ class "flex-spacer" ] []
          , button [ class "emoticons", onClick ToggleEmoticons ] []
          , input [ type_ "submit", class "submit", value "Send", onClick SendMessage ] []
            ]
          ]
        ]
      ]

