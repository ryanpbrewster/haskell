-- first_non_repeated_character.hs
{-
 - Write a program to find the first non repeated character in a string.
 - Input sample:
 -
 - The first argument will be a text file containing strings. e.g.
 -
 - yellow
 - tooth
 -
 - Output sample:
 -
 - Print to stdout, the first non repeating character, one per line.
 - e.g.
 -
 - y
 - h
 -}


import System.Environment (getArgs)
import Data.Maybe

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = lines txt
                       outputs = map (:"") $ mapMaybe firstUnique inputs
                   in unlines outputs


firstUnique [] = Nothing
firstUnique (ch:r) | ch `elem` r = firstUnique $ filter (/= ch) r
                   | otherwise  = Just ch
