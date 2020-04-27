
-- types.hs
--- hold all type information

module Types
( Position
, Grid
, Name
, Intro
, Battlecry
, Outro
, Health
, Worth
, Damage
, Protection
, Action
) where

-- purpose: used to give a x and y axis on the grid
type Position = (Int, Int)

-- purpose: used to hold all characters of the grid. can also be represented as [String]
type Grid = [[Char]]

-- purpose: used to store the name of an item, boss, level, etc.
type Name = String

-- purpose: used for the boss when combat is initiated
type Intro = String

-- purpose: used for the boss when attacking 
type Battlecry = String

-- purpose: used for the boss when it is defeated
type Outro = String

-- purpose: used to hold health for player, boss, etc. 
type Health = Int

-- purpose: used to hold the worth of an weapon, equipment or item 
type Worth = Int

-- purpose: used to hold the value of damage the weapon gives in an attack
type Damage = Int

-- purpose: used to hold the value of protection from the boss' attack
type Protection = Int

-- purpose: used to hold the state name (/ the action intended when ending on that state)
type Action = String
