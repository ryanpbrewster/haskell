-- racing_chars.hs
{-
 -  In this challenge you will be given a file where each line is a section of
 -  a race track with obstructions, gates and checkpoints. The goal is to find
 -  a way of passing this track, using the following rules:
 -      Each section contains only a single gate or a gate with a checkpoint.
 -      The race car can ride only through gates or checkpoints.
 -      You should prefer driving through checkpoint rather then a gate.
 -      The obstructions are represented by "#" (hash).
 -      The gates are represented by "_" (underscore).
 -      The checkpoints are represented by "C" .
 -  Input sample:
 -
 -  Your program should accept as its first argument a path to a filename. Each line in this file is a new segment of a race track. E.g.
 -      #########_##
 -      ########C_##
 -      #######_####
 -      ######_#C###
 -      #######_C###
 -      #######_####
 -      ######C#_###
 -      ######C_####
 -      #######_####
 -      #######_####
 -  Output sample:
 -
 -  Print out the way of passing this race track starting from the first line
 -  of the file. Use a "|" (pipe) for the straight, use a "/" (forward slash)
 -  for the left turn and use a "\" (back slash) for the right turn. E.g.
 -      #########|##
 -      ########/_##
 -      #######/####
 -      ######_#\###
 -      #######_|###
 -      #######/####
 -      ######/#_###
 -      ######|_####
 -      #######\####
 -      #######|####
 -
 -  Constraints:
 -  The number of lines in a file is 50.
 -  The width of a section is 12 chars. 
 -}

import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = mapPath txt

mapPath txt = let lns = lines txt
                  poss = map mostImportantPosition lns
                  diffs = differences poss
                  chrs = '|' : [ turnSymbol d | d <- diffs ]
              in unlines $ zipWith3 placeCharInStr lns chrs poss

mostImportantPosition str
    | 'C' `elem` str = fromJust $ elemIndex 'C' str
    | otherwise      = fromJust $ elemIndex '_' str

differences xs = zipWith (-) (tail xs) (init xs)

turnSymbol (-1) = '/'
turnSymbol ( 0) = '|'
turnSymbol ( 1) = '\\'

placeCharInStr str chr idx = (take idx str) ++ [chr] ++ (drop (idx+1) str)
