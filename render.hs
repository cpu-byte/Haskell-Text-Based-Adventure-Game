
-- render.hs
--- includes functions regarding the displaying of the level grid on the console

module Render
( render
, refresh
) where

import Types
import Utility

-- example: render ["...","...","..."] = "\n...\n...\n...\n"
-- purpose: this function is used for displaying the level grid to the console window
-- process: using intercalate, the function will combine all elements with a string with a new line.
render :: Grid -> IO ()
render grid = do
    putStr "\n"
    putStrLn $ intercalate "\n" grid

-- example: refresh (0, 0) 'x' ["...","...","..."] = ["x..","...","..."]
-- purpose: this function will change a character in a specific position from a 2D grid
-- process: jog through all elements of the y axis until the y position is the same, then supply the x axis content with the x position to the setAtPos function where a single element of a list is replaced at a specified index (x in this case)
refresh :: Position -> Char -> Grid -> Grid
refresh _ _ [] = []
refresh (x, y) val (z:zs)
    | y == 0    = (setAtPos val x z):zs
    | otherwise = z:refresh (x, (y - 1)) val zs

