
-- fsm.hs
---- primitive finite state machine with states and paths (transitions) used in the game

module Source.FSM
( State
, Path
, newState
, getStateAction
, newPath
, getPathFrom
, getPathTerm
, getPathTo
) where

import Source.Types

---
---     STATE, DATA TYPE
---

-- syntax: State "someActionName"
-- purpose: within the finite state machine scope, states make the end points of paths. each state will have a string which will correlates to an action that can be compared
data State = State Action deriving (Eq, Show)

-- example: newState "examine"
-- purpose: this function is used when creating the datatype
-- process: the input of the function is used for the creation of the data type
newState :: Action -> State
newState output = State output

-- example: getStateAction examineAction
-- purpose: retrieve the value from the passed in data type
-- process: using pattern matching of the data type, the specific value is extracted
getStateAction :: State -> Action
getStateAction (State action) = action


---
---     PATH, DATA TYPE
---

-- syntax: Path currentState term stateWhenTerm
-- purpose: within the finite state machine scope, the paths are what connects the states together. paths are executed on the basis of the term used.
-- notes: typically these are referred to as "transitions" but I opted for a shorter definition
data Path = Path State Term State deriving (Eq, Show)

-- example: newPath (newState "idle") "hit" (newState "fight")
-- purpose: this function is used when creating the datatype
-- process: the input of the function is used for the creation of the data type
newPath :: State -> String -> State -> Path
newPath from term to = Path from term to

-- purpose: retrieve the value from the passed in data type
-- process: using pattern matching of the data type, the specific value is extracted
-- example: getPathFrom myPath
getPathFrom :: Path -> State
getPathFrom (Path from _ _) = from
-- example: getPathTerm myPath
getPathTerm :: Path -> String
getPathTerm (Path _ term _) = term
-- example: getPathTo myPath
getPathTo :: Path -> State
getPathTo (Path _ _ to) = to

