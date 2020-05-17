
-- level.hs
--- hold the level data type and all relevant functions

module Source.Level
( Level
, newLevel
, getLevelOrgn
, getLevelPrev
, getLevelNext
, getLevelObsr
, getLevelGrid
, getLevelStsh
, getLevelBoss
, setLevelStsh
, setLevelGrid
, setLevelBoss
, getLvl
, onStash
, searchStash
) where

import Source.Entity
import Source.Boss
import Source.Types

---
---     LEVEL
---

-- syntax: (0, 0) (3, 4) (5, 3) "Room made of stone" ["...","...","..."] (3, 4) [Entity, Entity] hardBoss
-- purpose: hold all information regarding a singular level in the game with locations which link to the previous and next levels in relation to all levels in the game
data Level = Level
    Position                -- origin / spawn point on level
    Position                -- location to previous level
    Position                -- location to next level
    String                  -- observable / description of the level
    Grid                    -- level
    Position                -- location to level stash
    [Entity]                -- contents of the level stash
    Boss                    -- boss of the level
    deriving (Show, Eq)

-- example: newLevel (0, 0) (3, 4) (5, 3) "Room made of stone" ["...","...","..."] (3, 4) [Entity, Entity] hardBoss
-- purpose: this function is used when creating the datatype
-- process: the input of the function is used for the creation of the data type
newLevel :: Position -> Position -> Position -> String ->
    Grid -> Position -> [Entity] -> Boss -> Level
newLevel origin prev next observ grid stashPos stashContent boss =
    Level origin prev next observ grid stashPos stashContent boss

-- purpose: retrieve the value from the passed in data type
-- process: using pattern matching of the data type, the specific value is extracted
-- example: myLvl = (1, 2)
getLevelOrgn :: Level -> Position
getLevelOrgn (Level o _ _ _ _ _ _ _) = o
-- example: myLvl = (10, 4)
getLevelPrev :: Level -> Position
getLevelPrev (Level _ p _ _ _ _ _ _) = p
-- example: myLvl = (5, 3)
getLevelNext :: Level -> Position
getLevelNext (Level _ _ n _ _ _ _ _) = n
-- example: myLvl = "Well lit cave with a lot of cob webs."
getLevelObsr :: Level -> String
getLevelObsr (Level _ _ _ d _ _ _ _) = d
-- example: myLvl = ["...","...","..."]
getLevelGrid :: Level -> Grid
getLevelGrid (Level _ _ _ _ g _ _ _) = g
-- example: myLvl = ((3, 5), [Entity, Entity])
getLevelStsh :: Level -> (Position, [Entity])
getLevelStsh (Level _ _ _ _ _ p c _) = (p, c) -- pos and contents
-- example: myLvl = Boss
getLevelBoss :: Level -> Boss
getLevelBoss (Level _ _ _ _ _ _ _ b) = b

-- purpose: get previous level and the value to update and create a new level based on old values except adding the new value
-- process: using pattern matching of the data type, the specific value can be ignored and a new level can be set with that value which is received as the second parameter
-- example: myLvl ["...","...","..."] = Level
setLevelGrid :: Level -> Grid -> Level
setLevelGrid (Level o p n d _ sp sc b) g  = (Level o p n d g sp sc b)
-- example: myLvl [Entity, Entity] = Level
setLevelStsh :: Level -> [Entity] -> Level
setLevelStsh (Level o p n d g sp _  b) sc = (Level o p n d g sp sc b)
-- example: myLvl newBoss = Level
setLevelBoss :: Level -> Boss -> Level
setLevelBoss (Level o p n d g sp sc _) b  = (Level o p n d g sp sc b)

-- example: getLvl 0 myLvls = Level
-- purpose: find a level as a specific index when given all levels
-- process: using the (!!) operator, find the item from the list of items
getLvl :: Int -> [a] -> a
getLvl idx lvls = lvls !! idx

-- example: (2, 3) myLvl = True
-- purpose: find if the position passed is the location of the stash
-- process: using the getLevelStsh, get the first of that pair and compare it with the position that is passed into the function
onStash :: Position -> Level -> Bool
onStash pos lvl = (fst $ getLevelStsh lvl) == pos

-- example: Level "master sword" = [Entity] 
-- purpose: from the entire level stash, find items based on the query given
-- process: from the level stash, retrieve all items. additionally, using the with words amount function, add the amount of words each of the items is as a pair. with that information, pass the query and the pair into the searchEntity function
searchStash :: Level -> [Char] -> [Entity]
searchStash lvl cmd = searchEntity cmd $ entityWithWrdAmt $ snd $ getLevelStsh lvl
