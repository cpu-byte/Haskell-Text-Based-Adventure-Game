
-- abilities.hs
--- contains all side abilities (except combat/attack) and the relevant functions

module Source.Abilities
( stashTake
, stashDrop
, displayStash
, displayInventory
, equipItem
, healPlayer
, gameHelp
, gameExit
, gameFinished
) where

import Source.Player
import Source.Entity
import Source.Configurable
import Source.Level
import Source.Utility
import Source.Types


---
---     TAKE, PLAYER ABILITY
---

-- example: stashTake (1, 2) myLvl myPlr "pick up the short sword"
-- purpose: take an item from the stash and place it in the player's inventory
-- process: this function primarily  checks if the passed position (where the user is) is a stash, if it is, proceed by calling the handleStashTake function
stashTake :: Position -> Level -> Player -> [Char] -> ((Level, Player), [Char])
stashTake pos lvl plr cmd
    | onStash pos lvl = handleStashTake lvl plr cmd
    | otherwise       = ((lvl, plr), "No stash nearby to take from.")

-- purpose: this is an extension of the stashTake function
-- process: first, check if there is any content from the search result, if there is, check if that move it from the stash of the level (newLvl), and add it to the player's inventory (newPlr)
handleStashTake :: Level -> Player -> [Char] -> ((Level, Player), [Char])
handleStashTake lvl plr cmd
    | (length res) == 0 = ((lvl, plr), "That item is not here.")
    | otherwise         = ((newLvl, newPlr), "It has been transfered to your inventory.")
    where
        res = searchStash lvl cmd
        newLvl = setLevelStsh lvl (filterEnt (head res) (snd (getLevelStsh lvl)))
        newPlr = setPlayerInventory plr ((head res):(getPlayerInventory plr))



---
---     DROP, PLAYER ABILITY
---

-- example: stashDrop (3, 4) myLvl myPlr "drop the short sword"
-- purpose: remove an item from inventory and place it in the stash contents of the level
-- process: this function primarily checks if the passed position (where the user is) is on stash, if it is, proceed by calling the handleStashDrop function
stashDrop :: Position -> Level -> Player -> [Char] -> ((Level, Player), [Char])
stashDrop pos lvl plr cmd
    | onStash pos lvl = handleStashDrop lvl plr cmd
    | otherwise       = ((lvl, plr), "No stash nearby to drop to.")

-- purpose: this is an extension of the stashDrop function
-- process: first, check if there is any content from the search result, if there is, check if that item is equipped as the weapon or equipment. if it is, prompt the user that they cannot drop an item they have equipped. if it isn't equipped, then remove it from the player (newPlayer) and add it to the level (newLvl)
handleStashDrop :: Level -> Player -> [Char] -> ((Level, Player), [Char])
handleStashDrop lvl plr cmd
    | (length res) == 0         = ((lvl, plr), "You don't own that item.")
    | isEquipped plr (head res) = ((lvl, plr), "You cannot drop an equipped item.")
    | otherwise                 = ((newLvl, newPlr), "It has been transfered to the stash.")
    where
        res = searchInventory plr cmd
        newLvl = setLevelStsh lvl ((head res):(snd (getLevelStsh lvl)))
        newPlr = setPlayerInventory plr (filterEnt (head res) $ getPlayerInventory plr)


---
---     EXAMINE, PLAYER ABILITY
---

-- example: displayStash myPlr
-- purpose: display all items in the stash to the console window
-- process: if the player is on the stash, fetch the contents of the level's stash and using gatherEntities, display all of the items in the console window
displayStash :: Position -> Level -> IO ()
displayStash pos lvl = do
    let str | onStash pos lvl = "\nAvaliable loot:\n" ++
                (gatherEntities $ reverse $ snd $ getLevelStsh lvl)
            | otherwise = "\nThere is no loot nearby."
    putStrLn str


---
---     INVENTORY, PLAYER ABILITY
---

-- example: displayInventory myPlr
-- purpose: list all items in the player's inventory including the item name, damage/protection, and worth. also include the player's HP
-- process: create constants with the various required data, and use putStrLn with those variables and strings to display formatted text to the console
displayInventory :: Player -> IO ()
displayInventory player = do

    -- getting player inventory items
    let
        nm = getPlayerName player
        hp = getPlayerHealth player
        we = getPlayerWeapon player
        eq = getPlayerEquipment player
        inv = getPlayerInventory player

    -- showing player name and HP
    putStrLn $ "\n" ++ nm ++ ", you are on " ++ (show hp :: String) ++ " health."

    -- showing all items in inventory
    putStrLn $ "\nCurrent weapon:\n" ++ (strEntity we)
    putStrLn $ "\nCurrent equipment:\n" ++ (strEntity eq)
    putStrLn $ "\nInventory:\n" ++ (gatherEntities $ reverse inv)


---
---     EQUIP, PLAYER ABILITY
---

-- example: equipItem myPlr "equip shield" = (myPlr, "Weapon changed!")
-- purpose: change the current weapon or equipment entity to another one
-- process: first, break down the command into an understandable parser input and search for the results that were found
equipItem :: Player -> [Char] -> (Player, [Char])
equipItem player command =
    equipItemInv player $ searchEntity command $ entityWithWrdAmt $ getPlayerInventory player

-- purpose: this is an extension of the equipItem function
-- process: first, change if there is any content from the search result, if there is, change the current weapon or equip only if the types match their intended loadout slots, i.e. ensure the entity is a weapon if the item to equip is a weapon, and ensure the entity is a piece of equipment if the item to equip is a piece of equipment. otherwise, let the player know that item cannot be equipped.
equipItemInv :: Player -> [Entity] -> (Player, [Char])
equipItemInv player items
    | length items == 0 = (player, "You do not have that item.")
    | otherwise         = changeInv player $ head items
    where changeInv player item
            | getEntityType item == 'W' = ((setPlayerWeapon player item), "Weapon changed!")
            | getEntityType item == 'E' = ((setPlayerEquipment player item), "Equipment changed!")
            | otherwise                 = (player, "You cannot equip an item.")


---
---     HEAL, PLAYER ABILITY
---

-- example: healPlayer myPlr = myPlr (now with 100 hp)
-- purpose: to refill the health points to 100
-- process: first check if the user is on 100hp, if so, prompt a message saying so. otherwise, return the player with 100 health and an appropriate message
healPlayer :: Player -> (Player, [Char])
healPlayer player
    | getPlayerHealth player == 100 = (player, "You have full health.")
    | otherwise                     = ((setPlayerHealth player 100), "You have been healed.")


---
---     HELP, PLAYER ABILITY
---

-- example: gameHelp
-- purpose: inform the user of all game commands and their objective
-- process: enter all information in multiple put string line commands with a wait to require the user to acknowledge the prompt because moving to the next section
gameHelp :: IO ()
gameHelp = do

    -- objective
    putStrLn "\nObjective:"
    putStrLn "  1. You need to escape the cave by heading up (^)."
    putStrLn "  2. Try getting expensive items while in the cave."

    -- commands, stash
    putStrLn "\nWhile over a stash (?), you can:"
    putStrLn "  /examine, this will list all items in the stash."
    putStrLn "  /pick up (item name), you will transfer the item to your inventory."
    putStrLn "  /drop (item name), you will transfer the item to the stash."

    -- commands, explore
    putStrLn "\nWhile exploring the levels, you can:"
    putStrLn "  /sleep, this will heal you back to 100hp."

    -- commands, explore and combat
    putStrLn "\nWhile exploring or in combat, you can:"
    putStrLn "  /inventory, to see all your items."
    putStrLn "  /equip (item name), to equip an item as a weapon or equipment."
    putStrLn "  /quit, will exit the game."

    -- commands, combat
    putStrLn "\nWhile in combat, you can:"
    putStrLn "  /attack, you will attack the opponent."

    -- general notes / tips and tricks
    putStrLn "\nNotes:"
    putStrLn "  - When defeated, you leave battle with 1hp."
    putStrLn "  - You might not need to defeat a boss to progress."
    putStrLn "  - Your current weapon and equipment count towards your inventory."
    putStrLn "  - You can go back to previous levels by going down (v)"

    wait


---
---     EXIT / GAME FINISH, PLAYER ABILITY
---

-- example: gameExit
-- purpose: inform the user of the game shutting down
-- process: using a put string line in the console followed by a wait to require the user to acknowledge the prompt because closing the console
gameExit :: IO ()
gameExit = do
    putStrLn "\nShutting down..."
    wait

-- purpose: provide feedback to the user they have successfully finished the game, greeting them with their name and score
gameFinished :: Player -> IO ()
gameFinished plr = do
    clear 
    putStrLn ((getPlayerName plr) ++ "! You have escaped the dungeon!\n")
    putStrLn ("Score: $" ++ (show $ gatherEntitiesWorth $ getPlayerInventory plr) ++ " worth of stuff.")
    wait
    putStrLn "Hope you had fun, thank you very much for playing!"
    putStrLn "\nGame available on my GitHub account: @dropcmd"
    gameExit
