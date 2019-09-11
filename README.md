# :robot: Rickety Robots :robot:
A browser-based Ricochet Robots client written in Elm, to be paired with a backend written in Elixir. Includes chat, scorekeeping, and democratically-driven game management!

## Frontend Model (not finalized, documentation WIP)
  * **keys** : `Keys`. List of booleans representing active keyboard input
  * **user** : `User`. Information about the current user.
  * **users** : `List` of `User`s. Information about all users, including scores.
  * **chat** : `List` of `Chatline`s. A list of lines of text/usernames.
  * **messageInProgress** : `String`. Current chat input text.
  * **nameInProgress** : `String`. Current name-change input text.
  * **colorInProgress** : `String`. Current color-change input selection.
  * **boundaryBoard** : `Grid` of `Int`s. 16-by-16 array of arrays of integers, where the value determines how to print the board.
  * **goal** : `GoalSymbol`. Active goal symbol.
  * **goalList** : `List` of `Goal`s. List of all goals, including their symbols and positions.
  * **toggleStates** : `{`settings: `String`, activePoll: `String`, pollOptions: `String`, emoticons: `String`, countdown: `String}`. States for all toggle-able UI elements.
  * **countdown** : `Int`. 60s countdown timer that begins when one solution has been found.
  * **currentTimer** : `Int`. Count-up timer that restarts for each puzzle.
  * **robots** : `List` of `Robot`s. List of robots including color (ID), position, and legal (unblocked) moves.
  * **activeRobot** : `Maybe Robot`. Current selected robot for UI purposes.
  * **movesQueue** : `List` of `Move`s. Current stack of moves.

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
 * `demote <UID>` : Remove owner status from user
 * `mute <UID>` : Set mute status for user (no chat or polls)
 * `unmute <UID>` : Remove mute status from user
 * `kick <UID>` : Kick user from game.
 * `set_score <UID> <# points>` : Set score of user to integer value.
 * `reset_scores` : Immediately reset all scores to 0.
 * `reset_board` : Immediately reset board, robots, and goal.
 * `new_game` : Immediately choose new goal.
 * `poll_time <# sec>` : Time before poll expires. Default is 120 seconds. Minimum is 30 seconds.
 * `countdown_time <# sec>` : Time to find better solutions. Default is 60 seconds.
 * `puzzles_before_new_board <# puzzles>` : Number of puzzles before a new board is generated. Default is 10.
 * `min_moves <# moves>` : A single-robot solution below this number will not increment the scoreboard. Default is 0.


## Build
Compile javascript from root directory: ```elm make ./src/Robots.elm --output=./assets/js/main.js```

Start project server at `http://localhost:8000` with ```elm reactor```. Navigate to `http://localhost:8000/Robots.html` to see the project.