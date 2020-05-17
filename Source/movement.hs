
-- movement.hs
--- helper functions for 2D list

module Source.Movement
( validateMove
, checkY
, checkX
, checkTile
, translatable
, translate
) where

import Source.Types
import Source.Utility

-- example: ["###","###","###"] (0, 2)
-- purpose: used to check if the passed in positions are valid in relation to the grid
-- process: check if the 'y' exists, then check the 'x', then check if the character at that position is acceptable
validateMove :: Grid -> Position -> Bool
validateMove l p
    | (checkY l p) && (checkX l p) && (checkTile l p)   = True
    | otherwise                                         = False

-- example: ["###","###","###"] (_, 2)
-- purpose: ensure the player isn't trying to walk out of the playable map on the y axis
-- process: safely check if there is y many elements in the array, if y is less than or greater than the limit
checkY :: Foldable t => t a -> Position -> Bool
checkY l (_, y)
    | y >= 0 && y <= (length l - 2)         = True
    | otherwise                             = False

-- example: ["###","###","###"] (0, 2)
-- purpose: ensure the player isn't trying to walk out of the playable map on the x axis
-- process: assuming the existence of y, go that the y element and then safely check the item which in this case is the element on the x axis
checkX :: Foldable t => [t a] -> Position -> Bool
checkX l (x, y)
    | x >= 0 && x <= (length (l !! y) - 1)  = True
    | otherwise                             = False

-- example: ["###","###","###"] (1, 1)
-- purpose: check if the player isn't trying to navigate on top of a forbidden character
-- process: within a 2D array, on the 'x' and 'y' check if there are specific characters
checkTile :: Grid -> Position -> Bool
checkTile l (x, y)
    | elem ((l !! y) !! x) ['█', '|'] = False
    | otherwise                       = True

-- example: translatable !! 0 = 'w'
-- purpose: holds the keyboard characters which are used for navigation
-- process: function will return a list of the characters
translatable :: [Char]
translatable = ['w', 'W', 'a', 'A', 's', 'S', 'd', 'D']

-- example: translate 'w' (0, 2) = (0, -1)
-- purpose: translate the key pressed into updated positions
-- process: for each character, change the specific value in the position pair.
translate :: Char -> Position -> Position
translate t (x, y)
    | s == 'w' = (x, y - 1)
    | s == 'd' = (x + 1, y)
    | s == 's' = (x, y + 1)
    | s == 'a' = (x - 1, y)
    where s = head $ lowerCase (t:[])
