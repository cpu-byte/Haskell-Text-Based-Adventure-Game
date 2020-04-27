
-- entity.hs
--- module contains the entity data type and all relevent functions

module Entity
( Entity
, newWeapon
, newEquipment
, newItem
, getEntityType
, getEntityName
, getEntityWrth
, getWeaponDamage
, getEquipmentProtection
, strEntity
, strEntityRaw
, strEntityRawVal
, entityWithWrdAmt
, gatherEntities
, gatherEntitiesWorth
, searchEntity
, filterEnt
) where

import Utility
import Types


--- 
---     ENTITY
--- 

-- syntax: Weapon String Int Int
-- purpose: there are three types of items in this game, weapon, equipment and item. first two mentioned are equipable and give the player special ability to apply damage and to reduce the amount of damage taken
data Entity = Weapon    Name Worth Damage
            | Equipment Name Worth Protection
            | Item      Name Worth deriving (Eq, Show)

-- purpose: this function is used when creating the datatype
-- process: the input of the function is used for the creation of the data type
-- example: newWeapon "sword" 100 20
newWeapon :: Name -> Worth -> Damage -> Entity
newWeapon name desc damage = Weapon name desc damage
-- example: newEquipment "shield" 150 10
newEquipment :: Name -> Worth -> Protection -> Entity
newEquipment name desc protection = Equipment name desc protection
-- example: newItem "book" 200
newItem :: Name -> Worth -> Entity
newItem name desc = Item name desc

-- purpose: retrieve the value from the passed in data type
-- process: using pattern matching of the data type, the specific value is extracted
-- example: getEntityType myEnt = 'W'
getEntityType :: Entity -> Char
getEntityType (Weapon    _ _ _) = 'W'
getEntityType (Equipment _ _ _) = 'E'
getEntityType (Item      _ _)   = 'I'
-- example: getEntityName myEnt = "shield"
getEntityName :: Entity -> Name
getEntityName (Weapon    n _ _) = n
getEntityName (Equipment n _ _) = n
getEntityName (Item      n _)   = n
-- example: getEntityWrth myEnt = 120
getEntityWrth :: Entity -> Worth
getEntityWrth (Weapon    _ w _) = w
getEntityWrth (Equipment _ w _) = w
getEntityWrth (Item      _ w)   = w
-- example: getWeaponDamage myWeapon = 20
getWeaponDamage :: Entity -> Damage
getWeaponDamage (Weapon _ _ damage) = damage
-- example:  getEquipmentProtection myEquipment = 10
getEquipmentProtection :: Entity -> Protection
getEquipmentProtection (Equipment _ _ protection) = protection

-- example: strEntity myWeapon
-- purpose: before directly getting the string, this function provides some pre-sets which are applied to the strEntityRaw/Val function. presets depend if the item contains a special value: either damage or protection
-- process: using the strEntityRaw function, pass all parameters forward to the function in addition to some additional
strEntity :: Entity -> [Char]
strEntity (Weapon name worth dmg) =
    strEntityRawVal name worth dmg "damage"
strEntity (Equipment name worth ptc) =
    strEntityRawVal name worth ptc "protection"
strEntity (Item name worth) = strEntityRaw name worth

-- example: gatherEntities [Entity, Entity] = "...\n..."
-- purpose: display all items with their details
-- process: for each entity in the list, get its format string and combine in a singular string with new line between
gatherEntities :: [Entity] -> [Char]
gatherEntities [] = []
gatherEntities (x:xs) = ((strEntity x) ++ "\n") ++ (gatherEntities xs)

-- example: strEntityRaw "Short Sword" 100
-- purpose: return a formatted string ready for rendering on the console window
-- process: using concatenation, combine passed in data with characters
strEntityRaw :: Show a => [Char] -> a -> [Char]
strEntityRaw name worth =
    "  " ++ name ++ ", Worth $" ++ (show $ worth :: String)

-- example: strEntityRawVal "Short Sword" 100 "Damage" 20
-- purpose: return a formatted string ready for rendering on the console window
-- process: using concatenation, combine passed in data with characters. additionally, add an value with a specific name in the middle of the string.
strEntityRawVal :: (Show a1, Show a2) => [Char] -> a2 -> a1 -> [Char] -> [Char]
strEntityRawVal name worth val txt =
    "  " ++ name ++ ", " ++ (show $ val :: String) ++ " " ++ txt ++ ", Worth $" ++ (show $ worth :: String)

-- example: entityWithWrdAmt [Entity, Entity] = [(Entity, 3),(Entity, 1)]
-- purpose: this function is used for figuring out how many of the ending words need to be taken from the user input to compare with the items in the game.
-- process: for each entity in the list has its name split into the number of words. that value is combained with that actual entity with a pair 
entityWithWrdAmt :: [Entity] -> [(Entity, Int)]
entityWithWrdAmt ents = [ (x, length $ split $ getEntityName x) | x <- ents]

-- example: gatherEntitiesWorth [Entity, Entity] = 300
-- purpose: from a list of entities, retrieve the cumulative worth
-- process: using a generator, get all of the items, fetch the entity worth, and then add all items from the list with the sum operator
gatherEntitiesWorth :: [Entity] -> Int
gatherEntitiesWorth ents = sum [ getEntityWrth x | x <- ents ]

-- example: searchEntity "short sword" [(Entity, 2), (Entity, 1)]
-- purpose: perform a search on all entities of a passed in list
-- process:
--- 1. get the (lowercased) entity with the number of words of each item. ["short", "sword"] with length of 2
--- 2. make the query lowercase, then split it into words ["pick", "up", "short", "sword"]
--- 3. reverse it ["sword", "short", "up", "pick"]
--- 4. take as many words as the item being compared is. this case it is 2 for ["short", "sword"], thus: ["sword", "short"]
--- 5. then reverse it back: ["short", "sword"]
--- 6. compare it to the item being compared against: ["short", "sword"] == ["short", "sword"]
--- 7. if it equals the same, add it to the return list
searchEntity :: [Char] -> [(Entity, Int)] -> [Entity]
searchEntity cmd xs = [ ent | (ent, len) <- xs, hndlEnt ent == hndlCmd cmd len ]
    where
        hndlEnt x      = split (lowerCase (getEntityName x))
        hndlCmd cm len = reverse (take len (reverse $ split $ lowerCase cm))

-- example: filterEnt Entity [Entity, Entity] = Entity
-- purpose: remove certain entities from a list by specifying a blacklisted entity
-- process: every item in the list provided is checked if it is the forbidden entity. if it is, do not include it in the return list. if it isn't, include it in the return list
filterEnt :: Eq a => a -> [a] -> [a]
filterEnt _ [] = []
filterEnt ent (x:xs)
    | ent /= x  = x:(filterEnt ent xs)
    | otherwise = filterEnt ent xs
