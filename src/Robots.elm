port module Main exposing (Model, Msg(..), init, inputPort, main, outputPort, subscriptions, update, view)

import Coordinate exposing (..)
import Move exposing (Direction(..), Move)
import Board
import Color exposing (..)
import Robot exposing (Robot)
import Goal exposing (GoalSymbol(..), Goal)
import User exposing (User)
import Chat exposing (Chatline)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (style, type_, title, placeholder, value, class)
import Html.Events exposing (onInput, onSubmit, onClick)
import Time
import Tuple
import Set
import Json.Encode
import Json.Decode


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


type alias Model =
  { debugString : String
  , keys : Keys
  , user : User
  , users : List User
  , chat : List Chatline
  , messageInProgress : String
  , nameInProgress : String
  , colorInProgress : String
  , boundaryBoard : Board.Grid Int
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
  , backspace : Bool
  }


testFill : Int -> Int -> Int
testFill x y =
  1

init : () -> (Model, Cmd Msg)
init _ =
  (Model
    "Initialized model."
    (Keys False False False False False False False False False False False False False)
    { username = "patty", color = "#6c6adc", score = 0, owner = True, muted = False }
    [ ] -- `users` (and scores)
    [ ] -- `chat`
    "" -- `messageInProgress`
    "" -- `nameInProgress`
    "" -- `colorInProgress`
    (Board.square 16 (testFill) )                    -- boundaryBoard
    [ [217,145,147,153,145,145,145,145,145,145,145,147,153,145,145,179],
      [200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,54],
      [200,38,72,0,0,0,0,0,0,0,0,0,0,0,0,51],
      [238,93,128,0,0,0,0,0,0,0,0,0,0,0,0,50],
      [217,129,0,0,0,0,0,0,0,0,0,0,0,0,0,50],
      [200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50],
      [200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50],
      [200,0,0,0,0,0,0,256,257,0,0,0,0,0,0,50],
      [200,0,0,0,0,0,0,258,259,0,0,0,0,0,0,54],
      [204,0,0,0,0,0,0,0,0,0,0,0,0,0,0,51],
      [201,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50],
      [200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50],
      [200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50],
      [200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50],
      [200,0,0,0,0,0,0,0,0,0,0,0,0,0,0,50],
      [236,100,100,102,108,100,100,100,102,108,100,100,100,100,100,118] ]
-- [ [ 9,  3,  9,  1,  5, 65,  1,  1,  3,  9, 33,  5,  1,  1,  1,  3],
--       [ 8,  0,  0,  0,  3,  8,  0,  0,  0,  0,  2,  9,  0,  0,  0,  6],
--       [ 8,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  4, 67],
--       [10, 12,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  3, 10],
--       [24,  1,  0,  0, 32,  4,  0,  0,  0,  6,  8,  0,  0,  0,  0,  2],
--       [12,  0,  0,  0,  2,  9,  0,  0,  0,  1,128,  0,  0,  0,  0,  2],
--       [ 9,  0,  0,  6,  8,  0, 32,  4,  4, 64,  0,  2, 12,  0,  0,  2],
--       [ 8,  0,  4, 65,128,  0,  2, 99, 99,  8,  0, 16,  1,  0,  0,  2],
--       [ 8,  0,  3,  8,  0,  0,  2, 99, 99,  8,  0,  0,  0,  0,  0,  2],
--       [ 8,  0,  0,  0,  2, 12, 16,  1,  1,128, 32,  4,  0,  0,  0,  2],
--       [ 8,  6,  8,  0, 16,  1,  0,  0,  0,  0,  2,  9,  0,  0,  0,  2],
--       [ 8,  1,128,  0,  4, 64,  0,  0,  0,  0,  4, 64,  2, 12,  0,  6],
--       [12,  0,  0,  0,  3,  8,  0,  0,  0,  0,  3,  8, 16,  1,  0,  3],
--       [ 9,  0,  0,  0,  0, 32,  4,  0,  0,  0,  0,  0,  6,  8,  0,  2],
--       [ 8,  0,  0,  0,  0,  2,  9,  0,  0,  0,  0,  0,  1,128,  0,  2],
--       [12,  4,  4,  6, 12,  4,  4,  4,  4,  6, 12,  4,  4,  4,  4,  6] ]      -- board
    RedMoon                                                                   -- goalSymbol
    [ ]
    { settings = "none",
      pollOptions = "none",
      emoticons = "none",
      countdown = "flex" }                                                   -- toggleStates
    60                                                                       -- countdown
    0                                                                        -- currentTimer
    []                                                                       -- robots
    Nothing                                                                  -- activeRobot
    []                                                                       -- movesQueue
  ,  outputPort (Json.Encode.encode 0 (Json.Encode.object [ ("code", Json.Encode.int 200), ("content", User.encodeUser { username = "patty", color = "#6c6adc", score = 0, owner = True, muted = False }) ] ) ) -- initialize user?
  )



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
  | Ping Time.Posix
  | GetJSON Json.Encode.Value      -- Parse incoming JSON
  | GetBoard Json.Encode.Value     -- 100
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
      , outputPort (Json.Encode.encode 0 (Json.Encode.object [ ("code", Json.Encode.int 201), ("content", User.encodeUser newUser) ] ) ) )

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
                          ( "content", Chat.encodeChatline model.user newmsg 0 ) ] ) ) )

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

    Ping newTime ->
      ( { model | currentTimer = (model.currentTimer + 1) }
        , outputPort (Json.Encode.encode
                        0
                      ( Json.Encode.object
                      [ ( "code", Json.Encode.int 1),
                        ( "content", Json.Encode.string "ping" ) ] ) )
      )

    GetJSON json ->
      case (Json.Decode.decodeValue decodeJSON json) of
        Ok {code, content} ->
          case code of
            100 ->
              update (GetBoard content) model
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

    GetBoard json ->
      case (Json.Decode.decodeValue Board.decodeBoard json) of
        Ok board ->
          ( { model | boundaryBoard = board}, Cmd.none )
        Err _ ->
          ( { model | robots = []}, Cmd.none )

    GetRobotList json ->
      case (Json.Decode.decodeValue Robot.decodeRobotsList json) of
        Ok robotList ->
          ( { model | robots = robotList}, Cmd.none )
        Err _ ->
          ( { model | robots = []}, Cmd.none )
          
    GetGoalList json ->
      case (Json.Decode.decodeValue Goal.decodeGoalList json) of
        Ok goalList ->
          let
            activeGoal =
              case List.head (List.filter .active goalList) of
              Nothing ->
                RedMoon -- TODO: handle error!
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
      case (Json.Decode.decodeValue User.decodeUsersList json) of
        Ok usersList ->
          ( { model | users = usersList}, Cmd.none )
        Err _ ->
          ( { model | users = []}, Cmd.none )

    GetUser json ->
      case (Json.Decode.decodeValue User.decodeUser json) of
        Ok user ->
          ( { model | user = user}, Cmd.none )
        Err _ ->
          ( { model | users = []}, Cmd.none )

    GetChat json ->
      case (Json.Decode.decodeValue Chat.decodeChatList json) of
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
              "1"      -> (Robot.getByColor Red model.robots)
              "r"      -> (Robot.getByColor Red model.robots)
              "R"      -> (Robot.getByColor Red model.robots)
              "2"      -> (Robot.getByColor Green model.robots)
              "g"      -> (Robot.getByColor Green model.robots)
              "G"      -> (Robot.getByColor Green model.robots)
              "3"      -> (Robot.getByColor Blue model.robots)
              "b"      -> (Robot.getByColor Blue model.robots)
              "B"      -> (Robot.getByColor Blue model.robots)
              "4"      -> (Robot.getByColor Yellow model.robots)
              "y"      -> (Robot.getByColor Yellow model.robots)
              "Y"      -> (Robot.getByColor Yellow model.robots)
              "5"      -> (Robot.getByColor Silver model.robots)
              "s"      -> (Robot.getByColor Silver model.robots)
              "S"      -> (Robot.getByColor Silver model.robots)
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
              "Backspace"  -> popMove model.movesQueue
              _ -> model.movesQueue
          else
            model.movesQueue
      in
        ( { model | keys = newKeys, debugString = debugStr, activeRobot = activeRobot, movesQueue = newQueue }, Cmd.none )

    -- Set active robot color for next move
    SetActiveRobot color ->
      ( { model | activeRobot = Robot.getByColor color model.robots}, Cmd.none )
    
    -- Only push if there is an active robot and move is in set of legal moves.
    AddMove dir ->
      let
        newQueue = pushMove dir model.activeRobot model.movesQueue
      in
        ( { model | movesQueue = newQueue }, Cmd.none )
    
    -- Remove last move from queue (for Undo)
    PopMove ->
      let
        moves = popMove model.movesQueue
      in
      ( { model | movesQueue = moves }, Cmd.none )
      
    -- Remove all moves from queue and reset the active robot color
    ClearMoves ->
      ( { model | movesQueue = [], activeRobot = Nothing}, Cmd.none )


pushMove : Direction -> Maybe Robot -> List Move -> List Move
pushMove dir activeRobot oldQueue =
  case activeRobot of
    Nothing -> oldQueue
    Just robot ->
      if (List.member dir robot.moves) then
        (Move robot.color dir) :: oldQueue
      else
        oldQueue

popMove : List Move -> List Move
popMove oldQueue =
  case oldQueue of
    [] -> []
    a::b -> b


-- debug function:
printMoveList : List Move -> String
printMoveList moveList =
  case moveList of
    a::b ->
      (Color.toString (Just (.color a))) ++ ":" ++ Move.directionToString (.direction a) ++ " -> " ++ (printMoveList b)
    _ ->
      ""


updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
  case key of
    " "          -> { keys | space     = isDown }
    "ArrowUp"    -> { keys | up        = isDown }
    "ArrowLeft"  -> { keys | left      = isDown }
    "ArrowDown"  -> { keys | down      = isDown }
    "ArrowRight" -> { keys | right     = isDown }
    "Enter"      -> { keys | enter     = isDown }
    "1"          -> { keys | one       = isDown }
    "2"          -> { keys | two       = isDown }
    "3"          -> { keys | three     = isDown }
    "4"          -> { keys | four      = isDown }
    "5"          -> { keys | five      = isDown }
    "r"          -> { keys | one       = isDown }
    "g"          -> { keys | two       = isDown }
    "b"          -> { keys | three     = isDown }
    "y"          -> { keys | four      = isDown }
    "s"          -> { keys | five      = isDown }
    "R"          -> { keys | one       = isDown }
    "G"          -> { keys | two       = isDown }
    "B"          -> { keys | three     = isDown }
    "Y"          -> { keys | four      = isDown }
    "S"          -> { keys | five      = isDown }
    "Escape"     -> { keys | esc       = isDown }
    "Backspace"  -> { keys | backspace = isDown }
    _            -> keys


decodeJSON : Json.Decode.Decoder JSONMessage
decodeJSON =
  Json.Decode.map2
    JSONMessage
    (Json.Decode.field "code" Json.Decode.int)
    (Json.Decode.field "content" Json.Decode.value)


-- SUBSCRIPTIONS

port outputPort : (String) -> Cmd msg
port inputPort : (Json.Encode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 60000 Ping
    , Time.every 1000 Tick
    , inputPort GetJSON
    , Browser.Events.onKeyUp (Json.Decode.map (KeyChanged False) (Json.Decode.field "key" Json.Decode.string))
    , Browser.Events.onKeyDown (Json.Decode.map (KeyChanged True) (Json.Decode.field "key" Json.Decode.string))
    ]


-- VIEW

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
  [ div [ class "score__username", style "color" user.color, title "UID: TODO!" ]
    ((text user.username) ::
    (if user.owner then span [ class "owner", title "Owner" ] [] else span [] []) ::
    (if user.muted then span [ class "muted", title "Muted" ] [] else span [] []) ::
    [])
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


drawAll : Int -> Board.Grid Int -> List Robot -> List Goal -> List ( List (Html Msg) )
drawAll rowi board robots goals =
  if rowi < 16 then
    drawRow rowi 0 board robots goals :: drawAll (rowi + 1) board robots goals
  else
    []

drawRow : Int -> Int -> Board.Grid Int -> List Robot -> List Goal -> List (Html Msg)
drawRow rowi colj board robots goals =
  if colj < 16 then
    drawSquare rowi colj board robots goals :: drawRow rowi (colj+1) board robots goals
  else
    []

drawSquare : Int -> Int -> Board.Grid Int -> List Robot -> List Goal -> Html Msg
drawSquare rowi colj board robots goals =
  let
    val = Board.get (colj, rowi) board
    robotSquares = List.map .pos robots
    matchedRobot = 
      case (List.head (List.filter (Robot.matchRobot rowi colj) robots)) of
        Nothing ->
          ""
        Just matchedRobotObj ->
          (Color.toString (Just (.color matchedRobotObj)))

    matchedGoal = 
      case (List.head (List.filter (Goal.matchGoal rowi colj) goals)) of
        Nothing ->
          ""
        Just matchedGoalObj ->
          (.filename (Goal.toString matchedGoalObj.symbol))
  in
    case val of
      Nothing ->
        div [ class "square square--block" ]
         [
           div [ class ("goal "++matchedGoal) ] []
         , div [ class ("robot robot--"++matchedRobot) ] []
        ]
      Just n -> 
        div [ class ("square square--" ++ String.fromInt n) ]
         [
           div [ class ("goal "++matchedGoal) ] []
         , div [ class ("robot robot--"++matchedRobot) ] []
         ]




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
  , input [ type_ "text", onInput SetName, placeholder "New name", value model.nameInProgress ] []
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
  , div [ class "poll__info" ] [ text ( "Use /poll <command> or /set <command> to change settings. UIDs can be found by hovering over usernames in the scoreboard." ) ]
  , div [ class "poll__command", title "Give 'owner' status to user. Owners can use '/set' to instantly change settings." ] [ text "owner ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", title "Remove 'owner' status from user." ] [ text "demote ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", title "Mute user. Muted users cannot chat or create polls." ] [ text "mute ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", title "Unmute user." ] [ text "unmute ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", title "Kick user from the game." ] [ text "kick ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", title "Set score of user to some number." ] [ text "set_score ", span [ class "red" ] [ text "UID " ], span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", title "Reset all scores to 0." ] [ text "reset_scores" ]
  , div [ class "poll__command", title "Reset board walls, goals, and robot positions." ] [ text "reset_board" ]
  , div [ class "poll__command", title "Reset goal position." ] [ text "new_game" ]
  , div [ class "poll__command", title "Set time limit for polls in seconds. Must be at least 30." ] [ text "poll_time ", span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", title "Set time limit for finding new solutions. Must be at least 0."] [ text "countdown_time ", span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", title "Set number of puzzles before a new board is shuffled." ] [ text "puzzles_before_new_board ", span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", title "Single-robot solutions below this number will not add to score. Must be at least 0." ] [ text "min_moves ", span [ class "blue" ] [ text "int" ] ]
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
        , div [ class "debug" ]
          [ text ( Debug.toString (Board.get (1, 1) model.boundaryBoard) ++ "   " ++ model.debugString ++ "   " ++ (printMoveList (List.reverse model.movesQueue))) ]          
        , div [ class "sidebar__goal" ] [ div [ class ("goal " ++ .filename (Goal.toString model.goal)) ] [ ] ]
        , div [ class "timer", onClick (IncrementScore model.user) ]
          [ div [ class "timer__countdown" ]
            [ span [] [ text (formatTimer model.countdown) ]
            , div [ class "icon icon--timer", title "Countdown before best solution wins!" ] []
            ]
          , div [ class "timer__current-timer" ]
            [ span [] [ text (formatTimer model.currentTimer) ]
            , div [ class "icon icon--clock", title "Time spent on current puzzle" ] []
            ]
          , div [ class ("counter__moves" ++ (if Move.countMoves model.movesQueue > 0 then " active" else "")) ]
            [ span [] [ text (String.fromInt (Move.countMoves model.movesQueue)) ]
            , div [ class "icon icon--count", title "Number of moves in current solution attempt" ] []
            ]            
          , div [ class ("counter__robots" ++ (if Move.countRobots model.movesQueue > 0 then " active" else "")) ]
            [ span [] [ text (String.fromInt (Move.countRobots model.movesQueue)) ]
            , div [ class "icon icon--robot", title "Number of robots in current solution attempt" ] []
            ]
          ]
        ]
      , div [ class "main"] [
          div [ class "controls" ]
          [ div [ class "controls__robots" ]
            [ div [ class ("controls__robot controls__red" ++ (if (Robot.getColor model.activeRobot == Just Red) then " active" else "")), onClick (SetActiveRobot Red), title "Select red robot ([R] or [1])"] []
            , div [ class ("controls__robot controls__green" ++ (if (Robot.getColor model.activeRobot == Just Green) then " active" else "")), onClick (SetActiveRobot Green), title "Select red robot ([G] or [2])" ] []
            , div [ class ("controls__robot controls__blue" ++ (if (Robot.getColor model.activeRobot == Just Blue) then " active" else "")), onClick (SetActiveRobot Blue), title "Select red robot ([B] or [3])" ] []
            , div [ class ("controls__robot controls__yellow" ++ (if (Robot.getColor model.activeRobot == Just Yellow) then " active" else "")), onClick (SetActiveRobot Yellow), title "Select red robot ([Y] or [4])" ] []
            , div [ class ("controls__robot controls__silver" ++ (if (Robot.getColor model.activeRobot == Just Silver) then " active" else "")), onClick (SetActiveRobot Silver), title "Select red robot ([S] or [5])" ] []
            ]
          , div [ class "controls__directions" ]
            [ div [ class ("controls__button controls__left" ++ (if model.activeRobot == Nothing then " inactive" else "") ++ (if model.keys.left then " active" else "")), onClick (AddMove Left), title "Move current robot left" ] []
            , div [ class ("controls__button controls__up" ++ (if model.activeRobot == Nothing then " inactive" else "") ++ (if model.keys.up then " active" else "")), onClick (AddMove Up), title "Move current robot up" ] []
            , div [ class ("controls__button controls__right" ++ (if model.activeRobot == Nothing then " inactive" else "") ++ (if model.keys.right then " active" else "")), onClick (AddMove Right), title "Move current robot right" ] []
            , div [ class ("controls__button controls__down" ++ (if model.activeRobot == Nothing then " inactive" else "") ++ (if model.keys.down then " active" else "")), onClick (AddMove Down), title "Move current robot down" ] []
            , div [ class ("controls__button controls__undo" ++ (if model.keys.backspace then " active" else "") ++ (if List.isEmpty model.movesQueue then " inactive" else "")), onClick PopMove, title "Undo last move" ] []
            , div [ class ("controls__button controls__cancel" ++ (if model.keys.esc then " active" else "") ++ (if List.isEmpty model.movesQueue then " inactive" else "")), onClick ClearMoves, title "Clear current moves" ] []
            ]
           ]
        , div [ class "game" ] (model.boundaryBoard |> drawBoard)
        ]
      , div [ class "sidebar" ]
        [
          div [ class "chat" ]
            (h2 [] [ text "Chat" ] :: (List.reverse model.chat |> drawChat))
        , div [ class "sidebar__settings", style "display" model.toggleStates.settings ] (drawSettings model)
        , div [ class "sidebar__polloptions", style "display" model.toggleStates.pollOptions ] (drawPollOptions)
        , div [ class "message"]
          [ textarea [ class "message__box", onEnter SendMessage, onInput SetMessage, placeholder "Send a message", value model.messageInProgress, Html.Attributes.maxlength 255 ] []
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

