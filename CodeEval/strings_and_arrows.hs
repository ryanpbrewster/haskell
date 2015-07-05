{-
 - STRINGS AND ARROWS
 -CHALLENGE DESCRIPTION:
 -
 - You have a string composed of the following symbols: '>', '<',
 - and '-'. Your task is to find, count, and print to the output
 - a number of arrows in the string. An arrow is a set of the
 - following symbols: '>>-->' or '<--<<'. 

 -Note that one character may belong to two arrows at the same
 time. Such example is shown in the line #1.
 -
 -INPUT SAMPLE:
 -
 - The first argument is a path to a file. Each line includes
 - a test case with a string of different length from 10 to 250
 - characters. The string consists of '>', '<', and '-' symbols.
 -
 -For example:

     <--<<--<<
     <<>>--><--<<--<<>>>--><
     <-->>

     2
     4
     0
 - CONSTRAINTS:
 -
 - An arrow is a set of the following symbols: '>>-->' or '<--<<'.
 - One symbol may belong to two arrows at the same time.
 - The number of test cases is 40.
 -}

import System.Environment (getArgs)
import Data.List (tails, isPrefixOf)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt =
    let lns = lines txt
        anss = map (length . filter legit . tails) lns
    in unlines $ map show anss

legit str = any (`isPrefixOf` str) [ ">>-->", "<--<<" ]
