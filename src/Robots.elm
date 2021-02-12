port module Robots exposing (Model, Msg(..), init, inputPort, main, outputPort, subscriptions, update, view)

import Coordinate exposing (..)
import Move exposing (Direction(..), Move)
import Board
import Color exposing (..)
import Robot exposing (Robot)
import Goal exposing (..)
import User exposing (User)
import Chat exposing (Chatline)

import Browser
import Browser.Events
import Browser.Dom as Dom
import Dict
import Html exposing (..)
import Html.Attributes exposing (id, style, type_, attribute, placeholder, value, class, name, for)
import Html.Events exposing (onInput, onClick, onFocus, onBlur)
import Json.Decode
import Json.Encode
import Process
import Task
import Time


-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias JSONMessage = 
  { action : String 
  , content : Json.Encode.Value
  }


type alias Model =
  { debugString : String
  , room_name : String
  , keys : Keys
  , blockKeyShortcuts : Bool
  , user : User
  , users : List User
  , chat : List Chatline
  , messageInProgress : String
  , nameInProgress : String
  , colorInProgress : String
  , boundaryBoard : Board.Grid Int
  , goal : Maybe GoalSymbol
  , goalList : List Goal
  , toggleStates :
    { settings: String
    , pollOptions: String
    , emoticons: String
    , countdown: String
    }
  , countdown : Int
  , currentTimer : Int
  , solutionFound : Bool
  , robots : List Robot
  , activeColor : Maybe Color
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
testFill _ _ =
  0

init : () -> (Model, Cmd Msg)
init _ =
  (Model
    "Initialized model."
    ""
    (Keys False False False False False False False False False False False False False)
    False                                                                    -- Block keyboard shortcuts
    { username = "Patrick", nickname = "patty", color = "#6c6adc", score = 0, is_admin = True, is_muted = False, team = 0 }
    [ ]                                                                      -- `users` (and scores)
    [ ]                                                                      -- `chat`
    ""                                                                       -- `messageInProgress`
    ""                                                                       -- `nameInProgress`
    ""                                                                       -- `colorInProgress`
    (Board.square 16 testFill )                                              -- boundaryBoard
    Nothing                                                                  -- goalSymbol
    [ ]
    { settings = "none",
      pollOptions = "none",
      emoticons = "none",
      countdown = "flex" }                                                   -- toggleStates
    60                                                                       -- countdown
    0                                                                        -- currentTimer
    False                                                                    -- solutionFound
    []                                                                       -- robots
    Nothing                                                                  -- activeRobot
    []                                                                       -- movesQueue
  , Cmd.none )



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
  | SwitchToCountdown Json.Encode.Value
  | SwitchToTimer Json.Encode.Value
  | Tick Time.Posix
  | Ping Time.Posix
  | GetJSON Json.Encode.Value              -- Parse incoming JSON
  | ConnectToServer Json.Encode.Value      -- 000
  | GetBoard Json.Encode.Value             -- 100
  | GetRobotList Json.Encode.Value         -- 101
  | GetGoalList Json.Encode.Value          -- 102
  | GetUsersList Json.Encode.Value         -- 200
  | GetUser Json.Encode.Value              -- 201
  | GetChat Json.Encode.Value              -- 202
  | GetSVG Json.Encode.Value               -- 202
  | BlockKeyShortcuts Bool
  | KeyChanged Bool String
  | SetActiveColor (Maybe Color)
  | AddMove Direction
  | PopMove
  | ClearMoves
  | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of -- case Debug.log "MESSAGE: " msg of

    SetName name ->
      ( { model | nameInProgress = name }, Cmd.none )
      
    SetColor color ->
      ( { model | colorInProgress = color }, Cmd.none )
      
    UpdateSettings ->
      let
        oldUser = model.user
        oldUsers = model.users
        oldName = oldUser.nickname
        oldToggleStates = model.toggleStates
        newColor = model.colorInProgress
        newName = if List.member model.nameInProgress (List.map .nickname oldUsers) then
                    oldName
                  else
                    if String.length model.nameInProgress > 0 then model.nameInProgress else oldName
        newUser = { oldUser | nickname = newName, color = newColor }
        replaceUser testUser =
          if oldUser.nickname == testUser.nickname then
            { testUser | nickname = newName, color = newColor }
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
      , outputPort (Json.Encode.encode 0 (Json.Encode.object [ ("action", Json.Encode.string "update_user"), ("content", User.encodeUser newUser) ] ) ) )

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
                        [ ( "action", Json.Encode.string "update_chat"),
                          ( "content", Chat.encodeChatline model.room_name model.user newmsg 0 ) ] ) ) )

    NewGame -> -- TO DO!
      ( { model
        | currentTimer = 0
        , countdown = 0
        }
        , outputPort (Json.Encode.encode
                        0
                      ( Json.Encode.object
                      [ ( "action", Json.Encode.string "new_game"),
                        ( "content", Json.Encode.string "" ) ] ) ) )

    NewGoal newGoal ->
      ( { model | goal = Just newGoal }, Cmd.none )

    IncrementScore user ->
      let
        incrementScore testUser =
          if user.nickname == testUser.nickname then
            { testUser | score = testUser.score + 1 }
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
          | pollOptions = if oldToggleStates.pollOptions == "none" then "flex" else "none"
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
            | settings = if oldToggleStates.settings == "none" then "flex" else "none"
            , pollOptions = "none"
            , emoticons = "none" }
      in
        ( { model | toggleStates = newToggleStates }, Cmd.none )
      
    ToggleEmoticons ->
      let
        oldToggleStates = model.toggleStates
        newToggleStates =
          { oldToggleStates
          | emoticons = if oldToggleStates.emoticons == "none" then "flex" else "none"
          , pollOptions = "none"
          , settings = "none" }
      in
        ( { model | toggleStates = newToggleStates }, Cmd.none )
    
    InsertEmoticon str ->
      ( { model | messageInProgress = model.messageInProgress ++ " :" ++ str ++ ": " }, Cmd.none )

    {- ???????? -}
    DisplayCountdown status ->
      let
          oldToggleStates = model.toggleStates
          newToggleStates = { oldToggleStates | countdown = status }
      in
        ( { model | toggleStates = newToggleStates }, Cmd.none )
        
    SwitchToCountdown json ->
      case Json.Decode.decodeValue (Json.Decode.field "timer" Json.Decode.int) json of
        Ok time ->
          case Json.Decode.decodeValue (Json.Decode.field "countdown" Json.Decode.int) json of
            Ok countdown ->
              ( { model | debugString = "Countdown beings", countdown = countdown, currentTimer = time, solutionFound = True }, Cmd.none )
            Err _ ->
              ( { model | debugString = "Countdown time error" }, Cmd.none )
        Err _ ->
          ( { model | debugString = "Countdown time error" }, Cmd.none )
        
    SwitchToTimer json ->
      case Json.Decode.decodeValue (Json.Decode.field "timer" Json.Decode.int) json of
        Ok time ->
          case Json.Decode.decodeValue (Json.Decode.field "countdown" Json.Decode.int) json of
            Ok countdown ->
              ( { model | currentTimer = time, countdown = countdown, solutionFound = False }, Cmd.none )
            Err _ ->
              ( { model | debugString = "Countdown time error" }, Cmd.none )
        Err _ ->
          ( { model | debugString = "Timer time error" }, Cmd.none )

    Tick _ ->
      let
          countdownDisplay = if model.solutionFound then model.countdown-1 else model.countdown
          currentTimerDisplay = model.currentTimer + 1
      in
          ( { model | currentTimer = currentTimerDisplay, countdown = max 0 countdownDisplay }, Cmd.none )

    Ping _ ->
      ( { model | currentTimer = model.currentTimer + 1 }
        , outputPort (Json.Encode.encode
                        0
                      ( Json.Encode.object
                      [ ( "action", Json.Encode.string "ping"),
                        ( "content", Json.Encode.string "ping" ) ] ) )
      )

    GetJSON json ->
      case Json.Decode.decodeValue decodeJSON json of
        Ok {action, content} ->
          case action of
            "connect_to_server"   ->
              update (ConnectToServer content) model
            "update_board" ->
              update (GetBoard content) model
            "update_robots" ->
              update (GetRobotList content) model
            "update_goals" ->
              update (GetGoalList content) model
            "update_scoreboard" ->
              update (GetUsersList content) model
            "update_user" ->
              update (GetUser content) model
            "update_chat" ->
              update (GetChat content) model
            "player_chat_new_message" ->
              update (GetChat content) model
            "system_chat_new_message" ->
              update (GetChat content) model
            "system_chat_to_player_new_message" ->
              update (GetChat content) model
            "system_chat_svg" ->
              update (GetSVG content) model
            "switch_to_countdown" ->
              update (SwitchToCountdown content) model
            "switch_to_timer" ->
              update (SwitchToTimer content) model
            "clear_moves_queue" ->
              update ClearMoves model
            _ ->
              -- (Debug.log "Error: unknown code in JSON message" model, Cmd.none ) -- Error: missing code
              ( model, Cmd.none ) -- Error: missing code

        Err _ ->
          ( { model | debugString = "Bad JSON: " ++ Json.Encode.encode 0 json}, Cmd.none )

    GetBoard json ->
      case Json.Decode.decodeValue Board.decodeBoard json of
        Ok board ->
          ( { model | boundaryBoard = board}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Critical error getting new board"}, Cmd.none )

    GetRobotList json ->
      case Json.Decode.decodeValue Robot.decodeRobotsList json of
        Ok robotList ->
          ( { model | robots = robotList}, Cmd.none )
        Err _ ->
          ( { model | robots = [], debugString = "Critical error getting new robots"}, Cmd.none )
          
    GetGoalList json ->
      case Json.Decode.decodeValue Goal.decodeGoalList json of
        Ok goalList ->
          let
            activeGoal =
              case List.head (List.filter .active goalList) of
              Nothing ->
                Nothing
              Just anyGoal ->
                Just anyGoal.symbol 
          in
            ( { model
                | goalList = goalList
                , goal = activeGoal
              }, Cmd.none )
        Err _ ->
          ( { model | goalList = [], debugString = "Critical error getting new goals"}, Cmd.none )

    GetUsersList json ->
      case Json.Decode.decodeValue User.decodeUsersList json of
        Ok usersList ->
          ( { model | users = Dict.values usersList}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing userlist JSON"}, Cmd.none )

    GetUser json ->
      case Json.Decode.decodeValue User.decodeUser json of
        Ok user ->
          ( { model | user = user}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing user JSON"}, Cmd.none )
          
    ConnectToServer json ->
      case Json.Decode.decodeValue Json.Decode.string json of
        Ok room_name ->
          ( { model | room_name = room_name}
          , outputPort
            ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "get_user")
                , ("content", Json.Encode.string room_name) ] ))
          )
        Err _ ->
          ( { model | debugString = "Error parsing room name!"}, Cmd.none )


    GetChat json ->
      case Json.Decode.decodeValue Chat.decodeChatline json of
        Ok chatline ->
          ( { model | chat = chatline::model.chat}, jumpToBottom )
        Err _ ->
          ( { model | debugString = "Error parsing chat JSON"}, Cmd.none )


    GetSVG json ->
      case Json.Decode.decodeValue Chat.decodeSVGline json of
        Ok chatline ->
          ( { model | chat = chatline::model.chat}, Cmd.none )
        Err _ ->
          ( { model | debugString = "Error parsing chat JSON"}, Cmd.none )

    BlockKeyShortcuts bool ->
          ( { model | blockKeyShortcuts = bool}, Cmd.none )

    KeyChanged isDown key ->
      if model.blockKeyShortcuts then
        ( model, Cmd.none )
      else
        let
          newKeys = updateKeys isDown key model.keys
          activeColor =
            if isDown then
              case key of
                "1"      -> Just Red
                "r"      -> Just Red
                "R"      -> Just Red
                "2"      -> Just Green
                "g"      -> Just Green
                "G"      -> Just Green
                "3"      -> Just Blue
                "b"      -> Just Blue
                "B"      -> Just Blue
                "4"      -> Just Yellow
                "y"      -> Just Yellow
                "Y"      -> Just Yellow
                "5"      -> Just Silver
                "s"      -> Just Silver
                "S"      -> Just Silver
                "Escape" -> Nothing
                _        -> model.activeColor
            else
              model.activeColor

          command =
            if isDown then
              case key of
                "ArrowLeft"  -> Just (update (AddMove Left) model)
                "ArrowRight" -> Just (update (AddMove Right) model)
                "ArrowUp"    -> Just (update (AddMove Up) model)
                "ArrowDown"  -> Just (update (AddMove Down) model)
                "Escape"     -> Just (update ClearMoves model)
                "Backspace"  -> Just (update PopMove model)
                _ -> Nothing
            else
              Nothing
        in
          case command of
            Just cmd ->
              cmd
            _ ->
              ( { model | keys = newKeys, activeColor = activeColor }, Cmd.none )

    -- Set active robot color for next move
    SetActiveColor color ->
      ( { model | activeColor = color}, Cmd.none )
    
    -- Only push if there is an active robot and move is in set of legal moves.
    AddMove dir ->
      let
        newQueue = pushMove dir model.activeColor model.robots model.movesQueue
      in
        if model.movesQueue == newQueue then
          ( model, Cmd.none )
        else
          ( { model | movesQueue = newQueue }, 
            outputPort
              ( Json.Encode.encode
                0
                ( Json.Encode.object
                  [ ("action", Json.Encode.string "game_action")
                  , ("content", Json.Encode.object
                    [ ("action", Json.Encode.string "submit_movelist"),
                      ("content", Json.Encode.list Move.encodeMove (List.reverse newQueue))
                    ]
                  )
                ]
              )
            )
          )
    
    -- Remove last move from queue (for Undo)
    PopMove ->
      let
        newQueue = popMove model.movesQueue
      in
      ( { model | movesQueue = newQueue },
          outputPort
            ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "game_action")
                , ("content", Json.Encode.object
                  [ ("action", Json.Encode.string "submit_movelist"),
                    ("content", Json.Encode.list Move.encodeMove (List.reverse newQueue))
                  ]
                )
              ]
            )
          )
        )
      
    -- Remove all moves from queue and reset the active robot color
    ClearMoves ->
      ( { model | movesQueue = [], activeColor = Nothing}, 
          outputPort
            ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "game_action")
                , ("content", Json.Encode.object
                  [ ("action", Json.Encode.string "submit_movelist"),
                    ("content", Json.Encode.list Move.encodeMove [])
                  ]
                )
              ]
            )
          )
        )

    NoOp ->
          ( model, Cmd.none )

isLegalMove : Direction -> Maybe Color -> List Robot -> Bool
isLegalMove dir activeColor robots =
  case activeColor of
    Nothing -> False
    Just color ->
      case Robot.getByColor color robots of
         Nothing -> False
         Just activeRobot -> List.member dir activeRobot.moves 

pushMove : Direction -> Maybe Color -> List Robot -> List Move -> List Move
pushMove dir activeColor robots oldQueue =
  case activeColor of
    Nothing -> oldQueue
    Just color ->
      case Robot.getByColor color robots of
        Nothing -> oldQueue
        Just activeRobot ->
          if List.member dir activeRobot.moves then
            Move color dir :: oldQueue
          else
            oldQueue

popMove : List Move -> List Move
popMove oldQueue =
  case oldQueue of
    [] -> []
    _::b -> b


{-
-- debug function:
printMoveList : List Move -> String
printMoveList moveList =
  case moveList of
    a::b ->
      Color.toString (Just (.color a)) ++ ":" ++ Move.directionToString (.direction a) ++ " -> " ++ (printMoveList b)
    _ ->
      ""
-}

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
    (Json.Decode.field "action" Json.Decode.string)
    (Json.Decode.field "content" Json.Decode.value)

jumpToBottom : Cmd Msg
jumpToBottom =
    Process.sleep 200
      |> (\_ -> (Dom.getViewportOf "chat"
                |> Task.andThen (\info -> Dom.setViewportOf "chat" 0 info.scene.height)
                |> Task.attempt (\_ -> NoOp))
      )


-- SUBSCRIPTIONS

port outputPort : (String) -> Cmd msg
port inputPort : (Json.Encode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 50000 Ping
    , Time.every 1000 Tick
    , inputPort GetJSON
    , Browser.Events.onKeyUp (Json.Decode.map (KeyChanged False) (Json.Decode.field "key" Json.Decode.string))
    , Browser.Events.onKeyDown (Json.Decode.map (KeyChanged True) (Json.Decode.field "key" Json.Decode.string))
    ]


-- VIEW

formatTimer : Int -> String
formatTimer seconds =
  let
    sec = modBy 60 seconds
    hrs = (seconds // 60) // 60
    min = (seconds // 60) - (hrs * 60)
  in
    if hrs > 0 then
      (String.fromInt hrs ++ "h" ) ++ String.pad 2 '0' (String.fromInt min) ++ "m"
    else
      String.pad 2 '0' (String.fromInt min) ++ ":" ++ String.pad 2 '0' (String.fromInt sec)

drawScore : String -> User -> Html Msg
drawScore is_self user =
  div [ class "score" ] 
  [ div [ class "score__username", style "color" user.color  ]
    [ span [ attribute "tooltip" ("UID: " ++ user.username), attribute "flow" "right" ] [ text user.nickname ]
    , if is_self == user.username then span [ class "self", attribute "flow" "right", attribute "tooltip" "This is you!" ] [] else span [] []
    , if user.is_admin then span [ class "owner", attribute "flow" "right", attribute "tooltip" "Owner" ] [] else span [] []
    , if user.is_muted then span [ class "muted", attribute "flow" "right", attribute "tooltip" "Muted" ] [] else span [] []
    ]
  , text (String.fromInt user.score)
  ]

drawMessage : Chatline -> Html Msg
drawMessage message =
  case message.kind of
    2 -> (div [ class "chat__line" ] [ object [ class "chat__line--svg", attribute "data" message.message ] [ ] ])
    _ -> case message.user of
           Just user -> -- regular chat message
             ( div [ class "chat__line" ] 
               ( div [ class "chat__username", style "color" user.color ]
                 [ text user.nickname ] :: parseEmoticonHtml message.message )
             )
           _ -> -- system message
             (div [ class "chat__line" ] [ em [ class "chat__line--system" ] [ text message.message ] ])


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
    {- robotSquares = List.map .pos robots -- draw symbols at robot start positions? no-}
    matchedRobot = 
      case List.head (List.filter (Robot.matchRobot rowi colj) robots) of
        Nothing ->
          Nothing
        Just matchedRobotObj ->
          (Just (.color matchedRobotObj))

    matchedGoal = 
      case List.head (List.filter (Goal.matchGoal rowi colj) goals) of
        Nothing ->
          Nothing
        Just matchedGoalObj ->
          Just (.filename (Goal.toString (Just matchedGoalObj.symbol)))

    innerHTML = []
      |> ( case matchedRobot of
            Nothing -> identity
            mr -> (::) (div [ class ("robot robot--"++ Color.toString mr), onClick (SetActiveColor mr) ] []))
      |> ( case matchedGoal of
            Just mg -> (::) (div [ class ("goal "++mg) ] [])
            Nothing -> identity)

  in
    case val of
      Nothing ->
        div [ class "square square--block" ]
         innerHTML
      Just n -> 
        div [ class ("square square--" ++ String.fromInt n) ]
         innerHTML


drawEmoticon : String -> Html Msg
drawEmoticon str =
  div [ class ("emoticon emoticon--" ++ str), onClick (InsertEmoticon str) ] []

emoticonList : List (String)
emoticonList = [ "cool", "crazy", "damn", "geek", "grin", "huh", "lol", "love", "omg", "pout", "sad", "smile", "stars", "ugh", "waiting", "whoopsy", "wink", "wtf" ]
drawEmoticons : List (Html Msg)
drawEmoticons =
  List.map drawEmoticon emoticonList

drawSettings : Model -> List (Html Msg)
drawSettings model =
  [ h2 [ ] [ text "Settings" ]
  , div [ class "settings__flexbox" ]
  [ div [ class "setting__input" ] [ input [ type_ "text", onInput SetName, placeholder "New name", value model.nameInProgress, onFocus (BlockKeyShortcuts True), onBlur (BlockKeyShortcuts False) ] [] ]
  , div [ class "setting__input" ]
    [ select [ onInput SetColor ]
      [ option [ value "", style "color" "#707070" ] [ text "Change color" ]
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
    ]
  , div [class "setting__checkbox"]
    [ div [ class "checkbox" ]
      [ input [ type_ "checkbox", id "checkboxShowSystemChat" ] []
      , label [ for "checkboxShowSystemChat" ] []
      ],
      text "System messages"
    ]
  , div [class "setting__checkbox"]
    [ div [ class "checkbox" ]
      [ input [ type_ "checkbox", id "checkboxSound" ] []
      , label [ for "checkboxSound" ] []
      ],
      text "Sound"
    ]
  , div [ class "setting__submit" ] [ input [ type_ "submit", class "submit", value "Update", onClick UpdateSettings ] [] ]
    ]
  ]
  
drawPollOptions : List (Html Msg)
drawPollOptions =
  [ h2 [ ] [ text "Poll Commands" ]
  , div [ class "poll__info" ] [ text "Use /poll <command> or /set <command> to change settings. UIDs can be found by hovering over usernames in the scoreboard." ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Give 'owner' status to user. Owners can use '/set'." ] [ text "owner ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Remove 'owner' status from user." ] [ text "demote ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Mute user. Muted users cannot chat or create polls." ] [ text "mute ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Unmute user." ] [ text "unmute ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Kick user from the game." ] [ text "kick ", span [ class "red" ] [ text "UID" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Set score of user to some number." ] [ text "score ", span [ class "red" ] [ text "UID " ], span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Reset all scores to 0." ] [ text "reset_scores" ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Reset board walls, goals, and robot positions." ] [ text "reset" ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Reset goal position." ] [ text "new" ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Set time limit for polls in seconds. Must be at least 30." ] [ text "poll_time ", span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Set time limit for finding new solutions. Must be at least 0."] [ text "countdown_time ", span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "Set number of puzzles before a new board is shuffled." ] [ text "puzzles_before_new_board ", span [ class "blue" ] [ text "int" ] ]
  , div [ class "poll__command", attribute "flow" "left", attribute "tooltip" "1-robot solutions below this number will not add to score." ] [ text "min_moves ", span [ class "blue" ] [ text "int" ] ]
  ]

parseEmoticonHtml : String -> List (Html Msg)
parseEmoticonHtml str =
  let
    parseEmoticon ind1 ind2 teststr =
      let
        parsedStr = String.slice (ind1+1) ind2 teststr
      in
        if List.member parsedStr emoticonList then
          span [ class ("emoticon emoticon--" ++ parsedStr) ] [] :: parseEmoticonHtml (String.dropLeft (ind2+1) str)
        else
          text (":"++parsedStr) :: parseEmoticonHtml (String.dropLeft ind2 str)
  in
    case String.indexes ":" str of
      a::b::_ ->
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
      List.map drawMessage chat
    drawScores users =
      List.map (drawScore model.user.nickname) users
    drawBoard board =
      List.concat ( drawAll 0 board model.robots model.goalList )

  in
    div [ class "container" ]
      [
        div [ class "scorebar" ]
        [ h2 [] [ text "Scoreboard" ]
        , div [ class "scores", id "scores"] [ div [id "scores__inner"] (List.reverse (List.sortBy .score model.users) |> drawScores) ]
        -- , div [ class "debug" ]
        --   [ text (  model.debugString ++ "   ") ]
        , div [ class "sidebar__goal" ] [ div [ class ("goal " ++ .filename (Goal.toString model.goal)) ] [ ] ]
        , div [ class "controls" ]
          [ div [ class "controls__robots" ]
            [ div [ class ("controls__robot controls__red" ++ (if model.activeColor == Just Red then " active" else "")), onClick (SetActiveColor (Just Red)), attribute "flow" "right", attribute "tooltip" "Select red robot ([R] or [1])"] []
            , div [ class ("controls__robot controls__green" ++ (if model.activeColor == Just Green then " active" else "")), onClick (SetActiveColor (Just Green)), attribute "flow" "right", attribute "tooltip" "Select green robot ([G] or [2])" ] []
            , div [ class ("controls__robot controls__blue" ++ (if model.activeColor == Just Blue then " active" else "")), onClick (SetActiveColor (Just Blue)), attribute "flow" "right", attribute "tooltip" "Select blue robot ([B] or [3])" ] []
            , div [ class ("controls__robot controls__yellow" ++ (if model.activeColor == Just Yellow then " active" else "")), onClick (SetActiveColor (Just Yellow)), attribute "flow" "right", attribute "tooltip" "Select yellow robot ([Y] or [4])" ] []
            , div [ class ("controls__robot controls__silver" ++ (if model.activeColor == Just Silver then " active" else "")), onClick (SetActiveColor (Just Silver)), attribute "flow" "right", attribute "tooltip" "Select silver robot ([S] or [5])" ] []
            ]
          , div [ class "controls__directions" ]
            [ span [attribute "flow" "right", attribute "tooltip" "Move current robot left"] [div [ class ("controls__button controls__left" ++ (if not (isLegalMove Left model.activeColor model.robots) then " inactive" else "") ++ (if model.keys.left then " active" else "")), onClick (AddMove Left) ] []]
            , span [attribute "flow" "right", attribute "tooltip" "Move current robot up" ] [div [ class ("controls__button controls__up" ++ (if not (isLegalMove Up model.activeColor model.robots) then " inactive" else "") ++ (if model.keys.up then " active" else "")), onClick (AddMove Up)] []]
            , span [attribute "flow" "right", attribute "tooltip" "Move current robot right" ] [div [ class ("controls__button controls__right" ++ (if not (isLegalMove Right model.activeColor model.robots) then " inactive" else "") ++ (if model.keys.right then " active" else "")), onClick (AddMove Right)] []]
            , span [attribute "flow" "right", attribute "tooltip" "Move current robot down"] [div [ class ("controls__button controls__down" ++ (if not (isLegalMove Down model.activeColor model.robots) then " inactive" else "") ++ (if model.keys.down then " active" else "")), onClick (AddMove Down)] []]
            , span [attribute "flow" "right", attribute "tooltip" "Undo last move" ] [div [ class ("controls__button controls__undo" ++ (if model.keys.backspace then " active" else "") ++ (if List.isEmpty model.movesQueue then " inactive" else "")), onClick PopMove] []]
            , span [attribute "flow" "right", attribute "tooltip" "Clear current moves"] [div [ class ("controls__button controls__cancel" ++ (if model.keys.esc then " active" else "") ++ (if List.isEmpty model.movesQueue then " inactive" else "")), onClick ClearMoves ] []]
            ]
           ]
        , div [ class ("timer" ++ if model.solutionFound then " winner" else ""), onClick (IncrementScore model.user) ]
          [ div [ class ("timer__countdown " ++ (if model.solutionFound then "active" else "inactive")) ]
            [ span [attribute "flow" "right", attribute "tooltip" "Countdown before best solution wins!"] [div [ class "icon icon--timer"] []]
            , span [] [ text (formatTimer model.countdown) ]
            ]
          , div [ class ("timer__current-timer " ++ (if model.solutionFound then "inactive" else "active")) ]
            [ span [attribute "flow" "right", attribute "tooltip" "Time spent on current puzzle" ] [div [ class "icon icon--clock"] []]
            , span [] [ text (formatTimer model.currentTimer) ]
            ]
          , div [ class ("counter__moves" ++ (if Move.countMoves model.movesQueue > 0 then " active" else "")) ]
            [ span [attribute "flow" "right", attribute "tooltip" "Moves in current solution attempt" ] [div [ class "icon icon--count"] []]
            , span [] [ text (String.fromInt (Move.countMoves model.movesQueue)) ]
            ]            
          , div [ class ("counter__robots" ++ (if Move.countRobots model.movesQueue > 0 then " active" else "")) ]
            [ span [attribute "flow" "right", attribute "tooltip" "Robots in current solution attempt" ] [div [ class "icon icon--robot"] []]
            , span [] [ text (String.fromInt (Move.countRobots model.movesQueue)) ]
            ]
          ]
        ]
      , div [ class "main"] [
          div [ class "game" ] (model.boundaryBoard |> drawBoard)
        ]
      , div [ class "sidebar" ]
        [ h2 [] [ text "Chat" ]
        , div [class "chat", id "chat"] ( model.chat |> List.reverse |> drawChat )
        , div [ class ("sidebar__settings " ++ ("module-" ++ model.toggleStates.settings)) ] (drawSettings model)
        , div [ class ("sidebar__polloptions " ++ ("module-" ++ model.toggleStates.pollOptions)) ] drawPollOptions
        , div [ class "message"]
          [ textarea [ class "message__box", onEnter SendMessage, onInput SetMessage, placeholder "Send a message", value model.messageInProgress, Html.Attributes.maxlength 255, onFocus (BlockKeyShortcuts True), onBlur (BlockKeyShortcuts False) ] []
          , div [ class ("sidebar__emoticons " ++ ("module-" ++ model.toggleStates.emoticons)) ] drawEmoticons
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

