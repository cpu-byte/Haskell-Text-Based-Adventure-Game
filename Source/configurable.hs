
-- configurable.hs
--- all game content. without this file, the game would just be an engine without any "game"

module Source.Configurable
( entities
, lvls
, startingHealth
, startingInventory
) where

import Source.Types
import Source.Level
import Source.Boss
import Source.Entity

---
---     ENTITY DECLARATIONS
---

-- purpose: store information on all the items there are in the game that include various type. each item type has a unique constructor designed to store extra information specific to that item
-- notes:
--- there is a different constructor whether you want to create a weapon, equipment or an item. demonstrated below, are multiple of various types created.
--- items require a name and how much they are worth
--- weapons require a name, damage and how much they are worth
--- equipment require a name, protection and how much they are worth
entity001 = newWeapon       "Club"                                  30      30
entity002 = newItem         "Bag of Candles"                        45
entity003 = newItem         "Ruby"                                  150
entity004 = newWeapon       "Wooden Spoon"                          10      5
entity005 = newEquipment    "Mining Gear"                           30      10
entity006 = newItem         "Theory of Computation Text Book"       85
entity007 = newWeapon       "Short Sword"                           50      40
entity008 = newItem         "Ancient Scripture"                     320
entity009 = newItem         "Tree Sapling"                          45
entity010 = newWeapon       "Combat Knife"                          110     55
entity011 = newEquipment    "Silver Shield"                         125     20
entity012 = newItem         "Bag of Coins"                          150
entity013 = newWeapon       "Master Sword"                          400     80
entity014 = newEquipment    "Steel Chestplate"                      315     45
entity015 = newItem         "Haskell Instruction Manual"            25
entity016 = newWeapon       "Training Sword"                        20      15

-- purpose: to indicate how much 
startingHealth :: Health
startingHealth = 90

-- notes:
--- the first and second elements of this list contain the weapon and equipment present on the player at the beginning of the game, and thus the types have to match (due to, for example, an item or equipment not having a damage value).
startingInventory :: [Entity]
startingInventory = [entity004, entity005]

-- purpose: to provide a reference of all items to the other parts of the game.
entities :: [Entity]
entities = [entity001
           ,entity002
           ,entity003
           ,entity004
           ,entity005
           ,entity006
           ,entity007
           ,entity008
           ,entity009
           ,entity010
           ,entity011
           ,entity012
           ,entity013
           ,entity014
           ,entity015
           ,entity016]



---
---     LEVEL (& BOSS) DECLARATIONS
---

-- notes:
--- syntax for the creation of a level: newLevel (origin position) (position to go to previous level) (position to go to the next level) (description of the level) (grid layout) (position of the stash) (stash contents) (boss content)
--- syntax for the creation of a boss: newBoss (boss name) (position of the boss on the grid) (attack damage) (health) (restored health after player is defeated)
--- more comprahensive level and boss explaination can be found in their respective files

--  LEVEL 0
lvl0 :: Level
lvl0 = newLevel
    (50, 8) (0, 0) (45, 5)
    "you just woke up and aren't sure how you got here. The walls are cold and contain some minerals. Looks like a mine shaft. There seems to be a passage in the wall but it is full of cobwebs."
    ["██████████████████████████████████████████████████████"
    ,"████     ███  L ████     ████    ██  J ███████████████"
    ,"███ P    █      ██   E   ██  I   █      ██████████████"
    ,"█████|||███||||█████||||████||||███||||███████████████"
    ,"███                                     ███    ███████"
    ,"██                                       !   ^ ███████"
    ,"███                                     ███    ███████"
    ,"███████████████████████        ███████████████████████"
    ,"████████████████████ ?         ██████#####██████    ██"
    ,"████████████████████           ███####███######      █"
    ,"███████████████████████        ███#█████████████||||██"
    ,"                  ||                                ██"
    ,"                  ||                                ██"
    ,"████████████████████████████████████████████████||||██"
    ,"████████████████████████████████████████████████    ██"]
    (21, 8) [entity003, entity004, entity005, entity001]
    boss0
boss0 :: Boss
boss0 = newBoss "Bandit" (41, 5) 15 100 100
    "Hello, would you like this sword?... inside of you!"
    "I heard about people like you!"
    "... I underestimated you ..."

--  LEVEL 1
lvl1 :: Level
lvl1 = newLevel
    (21, 12) (21, 13) (6, 11)
    "The stone here is more rough. You see no path available in front of you, there seems to be a noise coming from the right-hand side."
    ["██████████████████████████████████████████████████████"
    ,"███████████████████████████#####██████████████████████"
    ,"███████████████████#######   ########█████████████████"
    ,"███████████                               ████████████"
    ,"██████                   ████               ██████████"
    ,"████                  ████████████            ████████"
    ,"██          █████||||████████████████        ##███████"
    ,"███       ? ██               ██████████       ████████"
    ,"████     ████                 ###███████     █████████"
    ,"████|  |████            ██     ██████████      ███████"
    ,"██        ████||||██   ████     ██████████||!||███████"
    ,"████  ^          ██     ██         ██████      ███████"
    ,"████    ##  ###███       █████      ████       ███████"
    ,"█████████####████    v    ██████           ####███████"
    ,"██████████████████████████████████     ███████████████"
    ,"██████████████████████████████████████████████████████"]
    (10, 7) [entity006, entity007, entity002]
    boss1
boss1 :: Boss
boss1 = newBoss "The Bandit King" (44, 10) 25 150 150
    "Gahahaha, it'll be but a mo-mo-mo-moment!"
    "Take this!"
    "... ghaa, I didn't train enough ..."

--  LEVEL 2
lvl2 :: Level
lvl2 = newLevel
    (16, 5) (13, 5) (23, 3)
    "Your footsteps echo in the massive cavern. There is a guard in your line of sight, but it doesn't react to seeing you. It seems like its minding its own business."
    ["██████████████████████████████████████████████████████"
    ,"██████████████████████████████████████████████████████"
    ,"█████████████#####██        ██###          ███████████"
    ,"███████#######   ##██  ^   ██##                  █████"
    ,"████######          ██                             ███"
    ,"███###       v       ██████                         ██"
    ,"████#                  ###               ###        ██"
    ,"████                                      ###      ███"
    ,"████                                    #  #       ███"
    ,"███          ###                                 █████"
    ,"████         #██         #####            ████████████"
    ,"█████         ██        ##███###    ███  !       █████"
    ,"█████████                 ███##    ██████    ?    ████"
    ,"█████████████                     █████████        ███"
    ,"██████████████████████████████████████████████████████"
    ,"██████████████████████████████████████████████████████"]
    (45, 12) [entity008, entity009, entity010, entity011, entity001]
    boss2
boss2 :: Boss
boss2 = newBoss "Scaldera 'Knuckle Master' Dhark" (41, 11) 22 300 300
    "Oh look, a little kid!"
    "Try this on this for size!"
    "... nooo, my shoes ... How could you??? ..."

--  LEVEL 3
lvl3 :: Level
lvl3 = newLevel
    (3, 12) (1, 12) (33, 3)
    "Looks like a storage sector. There appears to be large amount of loot here. But most of it is behind thick metal bars."
    ["██████████████████████████████████████████████████████"
    ,"██████████████####██████##?##  #██████████████████████"
    ,"████████████###?#  ||    ###       ███████████████████"
    ,"█████████████###   ||            ^ ███████████████████"
    ,"██████████████####████            ████████████████████"
    ,"███████████████████████    !    ██████████████████████"
    ,"#####      ██████████████|| ||████████████████████████"
    ,"##?       ||                        ██████████████████"
    ,"##       ||                         ||  #####█████████"
    ,"###     ||                          ||   #?###████████"
    ,"█████████                           || ######█████████"
    ,"                                    ██████████████████"
    ," v                                  ██████████████████"
    ,"██████████████████||||████████████████████████████████"
    ,"██████████████████    ████████████████████████████████"]
    (26, 1) [entity012, entity013, entity014, entity001, entity002]
    boss3
boss3 :: Boss
boss3 = newBoss "Srg. Ramrock" (27, 5) 40 250 250
    "How dare you storm into my quarters!"
    "You can't kill me!"
    "... I ... I can't believe this. WHO are you? ..."

--  LEVEL 4
lvl4 :: Level
lvl4 = newLevel
    (3, 13) (1, 14) (58, 12)
    "You see the sunlight and the gusts of fresh air hit you, you are nearly out... However, there is someone standing at the exit, and they look powerful..."
    ["███████████      ████████                                  "
    ,"████████████||||████████████████                           "
    ,"███████████      ████████████████████        /             "
    ," ||                         ██████████      ///            "
    ," ||                             ███████     ///   /        "
    ,"████                             ████        |   ///       "
    ,"███████#                          ||          ?  ///       "
    ,"█████████##                       ||              |        " 
    ,"███████████#                     !    ....                 "
    ,"██████████#                       ||    ...                "
    ,"████████                          ||     .....             "
    ,"████                             ████     .........        "
    ,"                             ###███████       ............^"
    ,"                       ######█████████             ........"
    ," v     ██████████████████████████████                      "
    ,"    ██████████████████████████████                         "
    ,"███████████████████████████████          ~~~---~~~         "
    ,"████████████████████████████          ~~~---~~~---~~~      "
    ,"█████████████████████████          ~~~---~~~---~~~---~~~   "]
    (46, 6) [entity015, entity016, entity002]
    boss4
boss4 :: Boss
boss4 = newBoss "Ganon" (33, 8) 60 400 400
    "How dare you storm through my quarters like that!"
    "You know so little..."
    "... you must be him ..."

-- purpose: to provide a reference of all items to the other parts of the game.
lvls :: [Level]
lvls = lvl0:lvl1:lvl2:lvl3:lvl4:[]
