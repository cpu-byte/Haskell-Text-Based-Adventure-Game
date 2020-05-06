
-- parser.hs
--- module holding all functions to do with the parsing of the user command

module Source.Parser
( processCommand
, findPath
, filterWrds
, excludes
, validChrs
, parseCommand
) where

import Source.FSM
import Source.Utility
import Source.Entity
import Source.Configurable

-- example: parseCommand "pick up sword" = "Take"
-- purpose: initiate the sequence of commands which are necessary for finding out what state the game should be in
-- process: first, filter out all invalid characters, make sure they are lowercase (to match all other names in the game), split the search terms into words, followed by eliminating any characters which aren't required to find out the nature of the command (nature of the command: whether it is to take or drop something, etc.)
parseCommand :: [Char] -> State
parseCommand command =
    processCommand paths noneState $ filterWrds $ split $ lowerCase $ validChrs command

-- example: processCommand paths noneState ["pick", "up", "sword"]
-- purpose: find the right state given a command, e.g. "None", "Take", "Heal", etc.
-- process: while keeping the current state updated with every iteration (findState), go through all of the words in the command and see if any paths match the same from state and term using findPath. if the result is none, then return noneState. otherwise, recur the function and making the found state the findState with the remaining of the command as the (x:xs)
processCommand :: [Path] -> State -> [String] -> State
processCommand pathList findState [] = findState
processCommand pathList findState (x:xs) = do
    let
        results = findPath pathList findState x
        toReturn
            | (length results) == 0 = noneState
            | otherwise             = getPathTo (head results)
    processCommand pathList toReturn xs

-- example: findPath myPaths pickUpState "up" = [Path, Path]
-- purpose: from all paths, find which of them contain the same starting state and term (to find their finish state) 
-- process: with list generation, return only paths which contain the same starting state and term by using guards and equality checks
findPath :: [Path] -> State -> String -> [Path]
findPath pathList findState findTerm =
    [ x | x <- pathList, (findState == getPathFrom x), (findTerm == getPathTerm x) ]

-- example: filterWrds ["pick", "up", "the", "short", "sword"] = ["pick", "up"]
-- purpose: specific to the command parsing, remove all words which aren't necessary for finding out what state is trying to be executed
-- process: with a pre-defined function: rawFilterExc(lude), pass through the words which need to be filtered.
filterWrds :: [String] -> [String]
filterWrds str =
    rawFilterExc excludes str []

-- example: excludes = ["at", "the", "short", "sword", ...]
-- purpose: provide a list full of words which aren't necessary for finding out what action the user wishes to take
-- process: in addition to some connectors (e.g. "at" and "the"), also add all of the item names in the game
excludes :: [String]
excludes = ["at", "the", "stash", "in"] ++ gameItems entities
    where
        gameItems [] = []
        gameItems (x:xs) = (split $ lowerCase $ getEntityName x) ++ (gameItems xs)

-- example: validChrs "h3llo th3re" = "hllo thre"
-- purpose: filter out all characters which aren't valid
-- process: with rawFilterInc(lude), supply the characers we wish to include as the first parameter, and the seond being the string. the last is the starting string
validChrs :: [Char] -> [Char]
validChrs str =
    rawFilterInc (' ':(['A'..'Z'] ++ ['a'..'z'])) str []

-- notes:
--- there are three states for each command thus multiple paths.
--- for a command with 2 words, i.e. "pick up", there will be 3 states:
--- path one: state 0 to state 1: "pick"
--- path two: state 1 to state 2: "up"
--- if we arrive at state 2, we successfully know that the action we wish to execute is to "Take"

-- start state: main beginning state for every function
noneState           = newState "None"

-- ending states
takeState           = newState "Take"
dropState           = newState "Drop"
examineState        = newState "Examine"
healState           = newState "Heal"
equipState          = newState "Equip"
inventoryState      = newState "Inventory"
quitState           = newState "Quit"
attackState         = newState "Attack"
helpState           = newState "Help"

-- notes:
--- to account for the middle states like the middle of "pick" and "up",
--- intermediate states are created
pickUpSubpath       = newState "None"
lookAroundSubpath   = newState "None"
layIntoState        = newState "None"


-- notes:
--- for the transitions (paths), a stating and ending state is specified in addition to the term which is what makes that path active and transfers the current state to the end state.
--- the parser FSM uses all paths combined which can be found at the bottom of this haskell file. paths are divded into each command, this is an optional step because I believe it will improve readability.
--- on the right hand side, the tick (square root symbol) is when the command is finalised. at an elipsis, there are more parts to the path

-- take commands
takePaths :: [Path]
takePaths = [(newPath noneState                 "take"          takeState)              -- √
            ,(newPath noneState                 "pick"          pickUpSubpath)          -- ...
            ,(newPath pickUpSubpath             "up"            takeState)]             -- √

-- drop commands
dropPaths :: [Path]
dropPaths = [(newPath noneState                 "drop"          dropState)]             -- √

-- examine commands
lookPaths :: [Path]
lookPaths = [(newPath noneState                 "examine"       examineState)           -- √
            ,(newPath noneState                 "search"        examineState)           -- √
            ,(newPath noneState                 "look"          lookAroundSubpath)      -- ...
            ,(newPath lookAroundSubpath         "around"        examineState)]          -- √

-- heal commands
healPaths :: [Path]
healPaths = [(newPath noneState                 "heal"          healState)              -- √
            ,(newPath noneState                 "sleep"         healState)]             -- √

-- equip commands
equipPaths :: [Path]
equipPaths = [(newPath noneState                "equip"         equipState)             -- √
             ,(newPath noneState                "hold"          equipState)]            -- √

-- inventory commands
inventoryPaths :: [Path]
inventoryPaths = [(newPath noneState            "inventory"     inventoryState)         -- √
                 ,(newPath noneState            "inv"           inventoryState)         -- √
                 ,(newPath noneState            "bag"           inventoryState)]        -- √

-- attack commands
attackPaths :: [Path]
attackPaths = [(newPath noneState               "attack"        attackState)            -- √
              ,(newPath noneState               "strike"        attackState)            -- √
              ,(newPath noneState               "charge"        attackState)            -- √
              ,(newPath noneState               "hit"           attackState)            -- √
              ,(newPath noneState               "stab"          attackState)            -- √
              ,(newPath noneState               "slay"          attackState)            -- √
              ,(newPath noneState               "lay"           layIntoState)           -- ...
              ,(newPath layIntoState            "into"          attackState)]           -- √

-- help commands
helpPaths :: [Path]
helpPaths = [(newPath noneState                 "help"          helpState)              -- √
            ,(newPath noneState                 "guidance"      helpState)]             -- √

-- quit commands
quitPaths :: [Path]
quitPaths = [(newPath noneState                 "quit"          quitState)              -- √
            ,(newPath noneState                 "exit"          quitState)]             -- √

-- all paths / commands (and variations of the command)
paths :: [Path]
paths = takePaths ++ dropPaths ++ lookPaths ++ healPaths ++ equipPaths ++
    quitPaths ++ inventoryPaths ++ attackPaths ++ helpPaths

