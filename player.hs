
-- player.hs
--- hold the player data type with all relevant functions

module Player
( Player
, newPlayer
, getPlayerName
, getPlayerHealth
, getPlayerWeapon
, getPlayerEquipment
, getPlayerInventory
, setPlayerHealth
, setPlayerEquipment
, setPlayerWeapon
, setPlayerInventory
, searchInventory
, isEquipped
) where

import Entity
import Types


---
---     PLAYER
---

-- syntax: Player "Ben" 100 Entity Entity [Entity]
-- purpose: store all relevant player data required for gameplay
data Player = Player Name Health Entity Entity [Entity] deriving (Eq, Show)

-- example: newPlayer "Ben" 100 Entity Entity [Entity]
-- purpose: this function is used when creating the datatype
-- process: the input of the function is used for the creation of the data type
newPlayer :: Name -> Health -> Entity -> Entity -> [Entity] -> Player
newPlayer name health weapon equipment inventory =
    Player name health weapon equipment inventory

-- purpose: retrieve the value from the passed in data type
-- process: using pattern matching of the data type, the specific value is extracted
-- example: getPlayerName myPlr = "Ben"
getPlayerName :: Player -> Name
getPlayerName       (Player n _ _ _ _) = n
-- example: getPlayerHealth myPlr = 100
getPlayerHealth :: Player -> Health
getPlayerHealth     (Player _ h _ _ _) = h
-- example: getPlayerWeapon myPlr = Entity
getPlayerWeapon :: Player -> Entity
getPlayerWeapon     (Player _ _ w _ _) = w
-- example: getPlayerEquipment myPlr = Entity
getPlayerEquipment :: Player -> Entity
getPlayerEquipment  (Player _ _ _ e _) = e
-- example: getPlayerInventory myPlr = [Entity]
getPlayerInventory :: Player -> [Entity]
getPlayerInventory  (Player _ _ _ _ i) = i

-- purpose: get previous player and the value to update and create a new player based on old values except adding the new value
-- process: using pattern matching of the data type, the specific value can be ignored and a new player can be set with that value which is received as the second parameter
-- example: setPlayerHealth myPlr 100 = Player
setPlayerHealth :: Player -> Health -> Player
setPlayerHealth     (Player n _ w e i) h = newPlayer n h w e i
-- example: setPlayerWeapon myPlr Entity = Player
setPlayerWeapon :: Player -> Entity -> Player
setPlayerWeapon     (Player n h _ e i) w = newPlayer n h w e i
-- example: setPlayerEquipment myPlr Entity = Player
setPlayerEquipment :: Player -> Entity -> Player
setPlayerEquipment  (Player n h w _ i) e = newPlayer n h w e i
-- example: setPlayerInventory myPlr [Entity] = Player
setPlayerInventory :: Player -> [Entity] -> Player
setPlayerInventory  (Player n h w e _) i = newPlayer n h w e i

-- example: searchInventory myPlr ["short", "sword"]
-- purpose: from the search query, return relevant items
-- process: from the player inventory, retrieve all items. additionally, using the with words amount function, add the amount of words each of the items is as a pair. with that information, pass the query and the pair into the searchEntity function
searchInventory :: Player -> [Char] -> [Entity]
searchInventory plr cmd = searchEntity cmd $ entityWithWrdAmt $ getPlayerInventory plr

-- example isUnequipped myPlr Entity
-- purpose: check if the item is either a player's current weapon or equipment
-- process: compare the passed in item with the current player weapon and inventory
isEquipped :: Player -> Entity -> Bool
isEquipped plr ent = ((getPlayerWeapon plr) == ent) || ((getPlayerEquipment plr) == ent)
