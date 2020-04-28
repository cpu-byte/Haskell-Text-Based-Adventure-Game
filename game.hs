
-- game.hs
--- holds the main structure of the game, including the game initiation, movement and combat.

module Game
( start
) where

import System.IO -- required for hidden user input, used in level navigation
import Boss
import Player
import Entity
import FSM
import Parser
import Utility
import Movement
import Level
import Render
import Configurable
import Abilities



---
---     START
---

start :: IO ()
start = do

    -- game introduction
    clear
    hSetEcho stdin True
    putStrLn "Welcome! What is your name?"
    name <- getLine
    clear

    let
        -- level setup
        srtLvlIdx = 0
        srtLvl = getLvl srtLvlIdx lvls

        -- player & inventory setup
        -- wooden spoon and mining gear
        inv = startingInventory
        plr = newPlayer name startingHealth (inv !! 0) (inv !! 1) inv
    
    -- story introduction
    putStrLn (name ++ ", " ++ getLevelObsr srtLvl)
    wait
    putStrLn "-> Heal up and head upwards and attempt to exit the cave.\n"
    putStrLn "-> Perform commands with forward slash (/).\n"
    putStrLn "-> Write /help for guidance."
    wait

    -- start
    gameRender lvls srtLvlIdx (getLevelGrid srtLvl) (getLevelOrgn srtLvl) plr



---
---     CORE
---

-- notes:
--- to identify the user on the grid, their first letter of the name they inputted is used.
gameRender :: [Level] -> Int -> [String] -> (Int, Int) -> Player -> IO ()
gameRender lvls idx grd pos plr = do 

    clear
    -- get the correct level, and then that grid.
    -- after, refresh the requested position with the first character of the player's name.
    render $ refresh pos (head $ getPlayerName plr) $ getLevelGrid $ getLvl idx lvls

    -- pass the loop to manage the user input
    gameInput lvls idx grd pos plr

-- purpose: handling the input of the user
-- notes:
--- lvls contain the main data compared to the grd which holds the current data. both are required in order to get the original tile from when the user moves from a position.
gameInput :: [Level] -> Int -> [String] -> (Int, Int) -> Player -> IO ()
gameInput lvls idx grd pos plr = do  

    -- input
    hSetEcho stdin False
    key <- getChar

    if elem key translatable -- is key either W, A, S, or D (case insensitive)
        -- then pass the game state data and the potential user position to gameMove
        then gameMove lvls idx grd pos (translate key pos) plr
    else if key == '/' -- user is requesting to perform a command
        -- 
        then gameCommand lvls idx grd pos plr
    else gameInput lvls idx grd pos plr

-- notes:
--- this function contains the transPos parameter which is the intended position the user wishes to go to. translated position as the axis change was already applied but might not be valid
gameMove :: [Level] -> Int -> [String] -> (Int, Int) -> (Int, Int) -> Player -> IO ()
gameMove lvls idx grd pos transPos plr = do
    let
        valid  = validateMove grd transPos -- indicates if the move is valid
        potPos = update valid pos transPos -- holds the value of where the next move is going to be (depending on 'valid')
        newIdx -- if the potential position is for the previous level, change the level index, otherwise, do not change.
            | potPos == (getLevelPrev $ getLvl idx lvls) = idx - 1 
            | potPos == (getLevelNext $ getLvl idx lvls) = idx + 1
            | otherwise                                  = idx
        newLvl = newIdx /= idx -- holds whether the next refresh will contain a new level
        newGrd -- depending on if there will be a new level rendered, either render that new level, perform normal behaviour
            | newLvl    = getLevelGrid $ getLvl newIdx lvls
            | otherwise = update valid grd $ refresh pos (head $ getPlayerName plr) $ getLevelGrid $ getLvl idx lvls
        newPos -- depending on if there will be a new level rendered, either return the origin location of the new level, or the requested positon by the user
            | newLvl    = getLevelOrgn $ getLvl newIdx lvls
            | otherwise = potPos
        boss = getLevelBoss $ getLvl idx lvls -- regardless of if there is a new level, load the correct boss to check the user's position

    -- user finished the game
    if newIdx == (length lvls) then do
        gameFinished plr
    else do

        -- accessing a new level
        if newLvl == True then do
            clear
            if (newIdx > idx) then do

                -- entering an upper level (and showing the level description)
                putStrLn "Entering next level...\n"
                putStrLn $ getLevelObsr $ getLvl newIdx lvls
            else do
                
                -- entering a lower level (not showing the level descripiton)
                putStrLn "Entering previous level."
            wait
            
            -- render that new level on the console window
            gameRender lvls newIdx newGrd newPos plr

        -- entering a boss on a level (which has more than 0 HP)
        else if newPos == (getBossPos boss) && (getBossHealth boss) > 0 then do
            clear
            gameCombatInit lvls idx grd pos newPos plr
        
        -- else, render the grid with the new move
        else
            gameRender lvls newIdx newGrd newPos plr

-- purpose: handling when user has pressed the / key, requesting to perform a command
gameCommand :: [Level] -> Int -> [String] -> (Int, Int) -> Player -> IO ()
gameCommand lvls idx grd pos plr = do

    -- get the input from the user
    putStr "> "
    hSetEcho stdin True
    command <- getLine

    -- perform different logic depending on what state the FSM was in after processing the user input
    case (getStateAction $ parseCommand command) of

        -- actions which do impact the state of the game data
        "Take"        -> do
            let -- new will contain ((updated level, updated player), appropriate message)
                new = stashTake pos (getLvl idx lvls) plr command
                newLvls = (setAtPos (fst $ fst new) idx lvls)
                newPlr = snd $ fst new
            putStrLn $ snd new
            wait
            gameRender newLvls idx grd pos newPlr
        "Drop"        -> do
            let -- new will contain ((updated level, updated player), appropriate message)
                new = stashDrop pos (getLvl idx lvls) plr command
                newLvls = (setAtPos (fst $ fst new) idx lvls)
                newPlr = snd $ fst new
            putStrLn $ snd new
            wait
            gameRender newLvls idx grd pos newPlr
        "Heal"        -> do
            let newPlayer = healPlayer plr
            putStrLn $ snd newPlayer
            wait
            gameRender lvls idx grd pos $ fst newPlayer
        "Equip"       -> do
            let newPlayer = equipItem plr command
            putStrLn $ snd newPlayer
            wait
            gameRender lvls idx grd pos $ fst newPlayer

        -- actions which do not impact the state of the game data
        "None"        -> do
            putStrLn "Unknown command."
            wait
            gameRender lvls idx grd pos plr
        "Inventory"   -> do
            displayInventory plr
            wait
            gameRender lvls idx grd pos plr
        "Examine"     -> do
            displayStash pos $ getLvl idx lvls
            wait
            gameRender lvls idx grd pos plr
        "Help"         -> do
            gameHelp
            gameRender lvls idx grd pos plr
        "Quit"        -> do
            gameExit
        _           -> do
            putStrLn "You cannot do that right now."
            wait
            gameRender lvls idx grd pos plr



---
---     COMBAT
---

-- purpose: the entry point for combat against a level boss
gameCombatInit :: [Level] -> Int -> [String] -> (Int, Int) -> (Int, Int) -> Player -> IO ()
gameCombatInit lvls idx grd pos newPos plr = do
    
    -- introduction
    putStrLn ("You've encountered " ++
        (getBossName $ getLevelBoss $ getLvl idx lvls) ++
        "! You cannot leave the battle or heal yourself." )
    wait

    -- display the battle cry of the boss
    let boss = getLevelBoss $ getLvl idx lvls
    putStrLn $ (getBossName boss) ++ ":\n\t\"" ++ (getBossIntro boss) ++ "\""
    wait

    -- initicate combat
    gameCombat lvls idx grd pos plr newPos


-- purpose: this is the active loop when combat is active (until either the player or boss has been defeated)
gameCombat :: [Level] -> Int -> [String] -> (Int, Int) -> Player -> (Int, Int) -> IO ()
gameCombat lvls idx grd pos plr newPos = do
    
    -- display the statistics of the current game
    gameCombatStats plr $ getLevelBoss $ getLvl idx lvls

    -- retrieve the user command
    putStr "\nWhat would you like to do?\n> "
    hSetEcho stdin True
    command <- getLine

    -- perform different logic depending on what state the FSM was in after processing the user input
    case (getStateAction $ parseCommand command) of

        -- actions which do impact the state of the game data
        "Equip"       -> do
            -- newPlayer will contain (newPlayer, appropriate message)
            let newPlayer = equipItem plr command
            putStrLn $ snd newPlayer
            wait
            gameCombat lvls idx grd pos (fst newPlayer) newPos
        "Attack"       -> do
            -- continue to the next stage of combat, player attack
            gameCombatPlrAttack lvls idx grd pos plr newPos

        -- actions which do not impact the state of the game data
        "None"        -> do
            putStrLn "Unknown command. Refer to /help for guidance."
            wait
            gameCombat lvls idx grd pos plr newPos
        "Inventory"   -> do
            displayInventory plr
            wait
            gameCombat lvls idx grd pos plr newPos
        "Help"         -> do
            gameHelp
            gameCombat lvls idx grd pos plr newPos
        "Quit"        -> putStrLn "Shutting down..."
        _             -> do
            putStrLn "You cannot do that right now."
            wait
            gameCombat lvls idx grd pos plr newPos

-- purpose: handling when the user initates the attack
gameCombatPlrAttack :: [Level] -> Int -> [String] -> (Int, Int) -> Player -> (Int, Int) -> IO ()
gameCombatPlrAttack lvls idx grd pos plr newPos = do

    let -- gather the information about the boss and how much damage the user can do
        boss = getLevelBoss $ getLvl idx lvls
        plrDmg = getWeaponDamage $ getPlayerWeapon plr
    
    putStrLn $ "\nYou attacked, and hit " ++ (getBossName boss) ++ " for " ++ (show plrDmg) ++ " damage."
    wait

    let -- update the information about the 
        newBoss = setBossHealth boss $ (getBossHealth boss) - plrDmg
        newLvlWithBoss = setLevelBoss (getLvl idx lvls) newBoss
    
    if isBossDefeated newBoss then do -- check if the boss has been defeated
        let -- if so, refresh the grid to include a representation of the boss as dead and pack that into the newLvl, and then newLvls constant 
            newLvl = setLevelGrid newLvlWithBoss $ (refresh (getBossPos newBoss) '†' $ getLevelGrid $ getLvl idx lvls)
            newLvls = setAtPos newLvl idx lvls
        clear
        putStrLn $ "You defeated " ++ (getBossName boss) ++ "!" -- provide feedback on the boss the user has defeated
        wait
        -- display the battle cry of the boss
        putStrLn $ (getBossName boss) ++ ":\n\t\"" ++ (getBossOutro boss) ++ "\""
        wait
        gameRender newLvls idx grd newPos plr -- if boss is defeated the go back to the main game loop (with the new position)
    else do
        -- continue to the next stage of comabt, boss attack
        gameCombatBossAttack (setAtPos newLvlWithBoss idx lvls) idx grd pos plr newPos

-- purpose: continuation of the attack - turn for the boss to inflict damage onto the player
gameCombatBossAttack :: [Level] -> Int -> [String] -> (Int, Int) -> Player -> (Int, Int) -> IO ()
gameCombatBossAttack lvls idx grd pos plr newPos = do
    let 
        -- get boss information
        boss = getLevelBoss $ getLvl idx lvls
        bossName = getBossName boss
        bossDmg = getBossDamage boss
        -- amt of protection provided by the equipment the user is wearing
        plrProtection = getEquipmentProtection $ getPlayerEquipment plr 
        -- ensure the amount of health to be removed from the player won't be a negative (if the armour is stronger than the attack)
        takeHealth
            | plrProtection >= bossDmg = 0
            | otherwise                = bossDmg - plrProtection

    -- display the battle cry of the boss
    putStrLn $ bossName ++ ":\n\t\"" ++ (getBossBttcry boss) ++ "\""
    wait

    -- display a specific message if your armour is stronger than the attack by the boss
    if takeHealth /= 0 then do putStrLn $ bossName ++ " attacked, you took " ++ (show takeHealth) ++ " damage."
    else do putStrLn $ bossName ++ " attacked, you've armour absorbed all the damage!"
    wait

    -- new player with the adjusted health
    let newPlr = setPlayerHealth plr $ (getPlayerHealth plr) - takeHealth

    -- check if the player has 0 health
    if ((getPlayerHealth newPlr) <= 0) then do
        let
            -- change player health to 1 (so its not 0 or below)
            returnedPlr = setPlayerHealth newPlr 1
            -- reset boss health to 100
            returnedLvls = (setAtPos (setLevelBoss (getLvl idx lvls) $ setBossHealth boss $ getBossFlHlth boss) idx lvls)
        clear
        putStrLn "You have been defeated!\n\nYou dodged in the nick of time to survive!"  -- provide feedback that they have died
        wait
        gameRender returnedLvls idx grd pos returnedPlr -- if player is defeated the go back to the main game loop (with the old position)
    else do
        -- continue to the next stage of combat, wait for user input
        gameCombat lvls idx grd pos newPlr newPos

-- purpose: shows boss and player health inc. their equipment
gameCombatStats :: Player -> Boss -> IO ()
gameCombatStats plr boss = do
    -- healths
    putStrLn ("\t\t\t" ++ (getBossName boss) ++ ", " ++ (show $ getBossHealth boss) ++ "hp <" )
    putStrLn ("> " ++ (getPlayerName plr) ++ ", " ++ (show $ getPlayerHealth plr) ++ "hp" )

    -- loadout
    putStrLn $ "\nYour loadout:\n" ++
        (strEntity $ getPlayerWeapon plr) ++ "\n" ++ (strEntity $ getPlayerEquipment plr)
