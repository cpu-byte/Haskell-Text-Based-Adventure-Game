
-- boss.hs
--- contains the boss data type and includes all relevant functions

module Source.Boss
( Boss
, newBoss
, getBossName
, getBossPos
, getBossDamage
, getBossHealth
, getBossFlHlth
, getBossIntro
, getBossBttcry
, getBossOutro
, setBossHealth
, isBossDefeated
) where

import Source.Types

---
---     BOSS, DATA TYPE
---

-- syntax: Boss String (Int, Int) Int Int Int String String String
-- purpose: hold all information regarding the boss that appears on every level
data Boss = Boss Name Position Damage Health Health Intro Battlecry Outro deriving (Eq, Show)

-- example: newBoss "Ben" (3, 4) 20 100 100 "..." "..." "..."
-- purpose: this function is used when creating the datatype
-- process: the input of the function is used for the creation of the data type
newBoss :: Name -> Position -> Damage -> Health -> Health -> Intro -> Battlecry -> Outro -> Boss
newBoss name pos dmg health flhlth intro battlecry outro =
    Boss name pos dmg health flhlth intro battlecry outro

-- purpose: retrieve the value from the passed in data type
-- process: using pattern matching of the data type, the specific value is extracted
-- example: getBossName myBoss = "Ben"
getBossName :: Boss -> Name
getBossName   (Boss n _ _ _ _ _ _ _) = n
-- example: getBossPos myBoss = (3, 4)
getBossPos :: Boss -> Position
getBossPos    (Boss _ p _ _ _ _ _ _) = p
-- example: getBossDamage myBoss = 25
getBossDamage :: Boss -> Damage
getBossDamage (Boss _ _ d _ _ _ _ _) = d
-- example: getBossHealth myBoss = 10
getBossHealth :: Boss -> Health
getBossHealth (Boss _ _ _ h _ _ _ _) = h
-- example: getBossFlHlth myBoss = 350
getBossFlHlth :: Boss -> Health
getBossFlHlth (Boss _ _ _ _ f _ _ _) = f
-- example: getBossIntro myBoss = "Text when combat is iniciated"
getBossIntro :: Boss -> Intro
getBossIntro  (Boss _ _ _ _ _ i _ _) = i
-- example: getBossBttcry myBoss = "Text before the boss attacks the user"
getBossBttcry :: Boss -> Battlecry
getBossBttcry (Boss _ _ _ _ _ _ b _) = b
-- example: getBossOutro myBoss = "Text when the boss is defeated"
getBossOutro :: Boss -> Outro
getBossOutro  (Boss _ _ _ _ _ _ _ o) = o

-- example: setBossHealth myBoss 100 = Boss
-- purpose: get previous boss and the value to update and create a new boss based on old values except adding the new value
-- process: using pattern matching of the data type, the specific value can be ignored and a new boss can be set with that value which is received as the second parameter
setBossHealth :: Boss -> Health -> Boss
setBossHealth (Boss n p d _ f i b o) h = Boss n p d h f i b o

-- example: isBossDefeated myBoss = False
-- purpose: check if the boss is defeated, to determine whether to end combat
-- process: check if the value received from calling get boss health is less than or equal to 0
isBossDefeated :: Boss -> Bool
isBossDefeated boss = (getBossHealth boss) <= 0
