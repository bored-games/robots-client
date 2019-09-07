# :robot: Rickety Robots :robot:
A browser-based Ricochet Robots client written in Elm, to be paired with a backend written in Elixir. Includes chat, scorekeeping, and democratically-driven game management!

## Frontend Model (not finalized, documentation WIP)
  * **keys** : `Keys`. List of active keyboard input
  * **user** : `User`. Information about the current user.
  * **users** : `List User`. Information about all users, including scores.
  * **chat** : `List Chatline`. A list of lines of text/usernames.
  * **messageInProgress** : `String`. Current chat input text.
  * **nameInProgress** : `String`. Current name-change input text.
  * **colorInProgress** : `String`. Current color-change input selection.
  * **boundaryBoard** : `Grid.Grid Color`. TODO.
  * **testboard** : `List ( List Int )`. TODO.
  * **goal** : `GoalSymbol`. Active goal.
  * **goalList** : `List Goal`. List of all goals.
  * **toggleStates** : `{settings: String, pollOptions: String, emoticons: String, countdown: String}`. States for all toggle-able UI elements.
  * **countdown** : `Int`. 60s countdown timer that begins when one solution has been found.
  * **currentTimer** : `Int`. Count-up timer that restarts for each puzzle.
  * **robots** : `List Robot`. List of robots including color (ID), position, and legal (unblocked) moves.
  * **activeRobot** : `Maybe Robot`. Current selected robot for UI purposes.
  * **movesQueue** : `List Move`. Current stack of moves.

## JSON Codes

### Backend -> Frontend
  * `001`: Tick...?
  ---
  * `100`: Reset `board` layout
  * `101`: Update `robots` positions and legal moves
  * `102`: Reset `goalList` positions
  * `103`: Update new goal
  * `104`: Switch to countdown (solution found)
  * `105`: Switch to clock (new game)
  * `106`: Send set of legal moves
  ---
  * `200`: Update `users`
  * `201`: Update `user`
  * `202`: Update `chat`
  * `202`: 
  * `203`: 

  
### Frontend -> Backend
  * `001`: Future: board click event...?
  ---
  * `100`: Reset all (scores, board, robot positions, goal)
  * `101`: Reset scores
  * `102`: Reset (shuffle) board
  * `103`: Reset robot positions
  * `104`: Reset goal
  * `105`: Submit move
  ---
  * `200`: Add user
  * `201`: Update user
  * `202`: Send (user) message
  * `203`: Send (system) message
  * `204`: Set score of user
  * `205`: 


## Keyboard Shortcuts
<kbd>1</kbd> or <kbd>R</kbd> : Select red robot

<kbd>2</kbd> or <kbd>G</kbd> : Select green robot

<kbd>3</kbd> or <kbd>B</kbd> : Select blue robot

<kbd>4</kbd> or <kbd>Y</kbd> : Select yellow robot

<kbd>5</kbd> or <kbd>S</kbd> : Select silver robot

<kbd>←</kbd>, <kbd>↑</kbd>, <kbd>→</kbd>, or <kbd>↓</kbd> : Attempt to queue a move for the selected robot in the corresponding direction.

<kbd>Esc</kbd> : Clear queue of moves

<kbd>Backspace</kbd> : Remove (undo) last move from queue

## Polls and Settings
Administration is performed through the use of `/poll <option> [param1, param2]` and `/set <option> [param1, param2]` commands entered through chat. A poll must succeed with more Yes votes than No votes to change settings. Only an **owner** can unilaterally change settings with `/set`. Available option/parameters:

 * `owner <UID>` : Grant owner status to user
 * `user <UID>` : Remove owner status from user
 * `mute <UID>` : Set mute status for user (no chat or polls)
 * `loud <UID>` : Remove mute status from user
 * `kick <UID>` : Kick user from game.
 * `set_score <UID> <int>` : Set score of user to integer value.
 * `reset_scores` : Immediately reset all scores to 0.
 * `reset_board` : Immediately reset board, robots, and goal.
 * `new_game` : Immediately choose new goal.
 * `poll_time <seconds>` : Time before poll expires. Default is 120 seconds. Minimum is 30 seconds.
 * `countdown_time <seconds>` : Time to find better solutions. Default is 60 seconds.
 * `puzzles_before_new_board <int>` : Number of puzzles before a new board is generated. Default is 10.
 * `min_moves <int>` : A single-robot solution below this number will not increment the scoreboard. Default is 0.


## Build
Compile javascript from root directory: ```elm make ./src/Robots.elm --output=./assets/js/main.js```

Start project server at `http://localhost:8000` with ```elm reactor```. Navigate to `http://localhost:8000/Robots.html` to see the project.