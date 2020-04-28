
-- utility.hs
--- holds common and misc. functions used by the program

module Utility
( update
, wait
, clear
, rawFilterInc
, rawFilterExc
, split
, rawSplit
, alphbt
, alphbtLw
, alphbtUp
, valAtIndex
, posOfVal
, lowerCase
, setAtPos
, rawSetAtPos
, intercalate
) where

-- example: update True 0 1 = 1
-- purpose: quick way to extract one of two values depending on a boolean value. without this function, there was a need to implment a guard expression into each of the functions which used this methodology
-- process: through guards, simple match the boolean parameter and return the equivalent value (second parameter: old when false, third parameter: new when true)
update :: Bool -> p -> p -> p
update valid old new | valid = new | otherwise = old

-- example: wait
-- purpose: slow the user down and require a key stroke before continuing with the program
-- process: print a statement to console to press an key while a x value is reading the input. no value is required, simply moving past the getLine will continue the parent algorithm
wait :: IO ()
wait = do
    putStrLn "\nPress enter to continue..."
    x <- getLine
    putStrLn ""

-- example: clear
-- purpose: wipe the current console output
-- process: print numerous new line statements
clear :: IO ()
clear = putStr "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"

-- example: rawFilterInc [1, 3, 5] [1, 2, 3, 4, 5] [] = [1, 3, 5]
-- purpose: remove items in a list that don't beling to a set / make a list of items which belong in a set, 'include' items
-- process: with guards, check if each value of the list to check exists in the set list, if it does, include it in the result
rawFilterInc :: (Foldable t, Eq a) => t a -> [a] -> [a] -> [a]
rawFilterInc set [] ys = reverse ys
rawFilterInc set (x:xs) ys
    | elem x set    = rawFilterInc set xs (x:ys)
    | otherwise     = rawFilterInc set xs ys

-- example: rawFilterInc [1, 3, 5] [1, 2, 3, 4, 5] [] = [2, 4]
-- purpose: remove items in a list that do belong in a set / make a list of items which don't belong in a set, 'exclude' them
-- process: with guards, check if each value of the list to check exists in the set list, if it does, do not include it in the result
rawFilterExc :: (Foldable t, Eq a) => t a -> [a] -> [a] -> [a]
rawFilterExc set [] ys = reverse ys
rawFilterExc set (x:xs) ys
    | elem x set    = rawFilterExc set xs ys
    | otherwise     = rawFilterExc set xs (x:ys)

-- example: split "hello there" = ["hello", "there"]
-- purpose: this function uses the rawSplit but presets some of the configurables in the mentioned function
-- process: call the rawSplit function with pre-set values
split :: [Char] -> [[Char]]
split str = rawSplit ' ' str [] []

-- example: rawSplit ' ' "hello there" [] [] = ["hello", "there"]
-- purpose: turn a string (possibily a sentence) into a list of strings (words)
-- process: param 2: working list. param 3: list of character not added to the words list (yet). param4: list of words to return. the main provided string breaks on a specific character: char. in that case, add all previous characters into the list to output (words). the pre list is used to add characters which are in the temporary storage before the specific character is seen. if there are characters remaining at the end of the working list, add those characters as a word to the return list (words)
rawSplit :: Eq a => a -> [a] -> [a] -> [[a]] -> [[a]]
rawSplit char [] [] words = reverse words
rawSplit char [] pre words = reverse ((reverse pre):words)
rawSplit char (x:xs) pre words
    | x == char     = rawSplit char (xs) [] ((reverse pre):words)
    | otherwise     = rawSplit char (xs) (x:pre) words

-- example: alphbt !! 2 = 'B'
-- purpose: provide a working list of characters to be used in typing commands into the game
-- process: using generators, create a lower and upper case alphabet and push a white space a the beginning
alphbt :: [Char]
alphbt = ' ':(alphbtLw ++ alphbtUp)
alphbtLw :: [Char]
alphbtLw = ['a'..'z']
alphbtUp :: [Char]
alphbtUp = ['A'..'Z']

-- example: valAtIndex "hello" 3 = 'l'
-- purpose: find a value at a position of a list with the added function of the requested length wrapped against the list when the length has exceeded.
-- process: using modulo, wrap the length as many times required until an acceptable value is found. with that value, find a element on the list with the !! operator.
valAtIndex :: [a] -> Int -> a
valAtIndex xs i = xs !! (i `mod` length xs)

-- example: posOfVal [1, 2, 3, 1] 1 = [0, 3]
-- purpose: find all the positions of an item in a list
-- process: generate a pair for each value of the list with the according count. for each value match, return the position of the pair
posOfVal :: (Num a1, Enum a1, Eq a2) => [a2] -> a2 -> [a1]
posOfVal xs x = [ i | (i, v) <- zip [0..] xs , v == x ]

-- example: "Hello There" = "hello there"
-- purpose: turn a list of characters into all lowercase characters
-- process: first, find the position of the item in an upper alphabet list. get the item from that list and provide it to the valAtIndex function which has the same alphabet generated but with lowercase characters. add that value to the return list. if the char is a lowercase, do not do the calculation
lowerCase :: [Char] -> [Char]
lowerCase [] = []
lowerCase (x:xs) = (calc x):(lowerCase xs)
    where calc x
            | elem x (alphbtLw) = x
            | otherwise         =
                valAtIndex (' ':alphbtLw) $ head $ posOfVal (' ':alphbtUp) x 

-- example: 99 [0, 1, 2, 3, 4] 2 = [0, 1, 99, 3, 4] 
-- purpose: given a list, replace one of the items at a specific index
-- process: initiate the rawSetAtPos function with pre-defined values
setAtPos :: (Eq a1, Num a1) => a2 -> a1 -> [a2] -> [a2]
setAtPos elm idx xs = rawSetAtPos elm idx xs 0 []

-- example: 99 [0, 1, 2, 3, 4] 2 0 [] = [0, 1, 99, 3, 4] 
-- purpose: given an element, an index, and a list, replace an item on the list with the element provided at the index specified. with this function, the starting index and the starting list is also specified.
-- process: iterate through an index, when hitting that index, add the requested item to the return list, otherwise, add the item from the original list.
rawSetAtPos :: (Eq a1, Num a1) => a2 -> a1 -> [a2] -> a1 -> [a2] -> [a2]
rawSetAtPos elm idx []     i final = reverse final
rawSetAtPos elm idx (x:xs) i final
    | idx == i  = rawSetAtPos elm idx xs (i + 1) (elm:final)
    | otherwise = rawSetAtPos elm idx xs (i + 1) (x:final)

-- example: intercalate "\n" ["hello", "there"] = "hello\nthere"
-- purpose: tie up a list of elements that contains a special value in between
-- process: get the first item from the list, add the seperator value after it, and add that into a pre-existing list. that list is a recursion method with the rest of the list and the same seperator
intercalate s [] = []
intercalate s (x:xs) = x ++ s ++ (intercalate s xs)

