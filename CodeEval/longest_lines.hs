-- longest_lines.hs
{-
 - Write a program to read a multiple line text file and write the 'N' longest
 - lines to stdout. Where the file to be read is specified on the command line.
 -
 - Input sample:
 -
 - Your program should read an input file (the first argument to your program).
 - The first line contains the value of the number 'N' followed by multiple
 - lines. You may assume that the input file is formatted correctly and the
 - number on the first line i.e. 'N' is a valid positive integer.e.g.
 -
 - 2
 - Hello World
 -
 - CodeEval
 - Quick Fox
 - A
 - San Francisco
 -
 - NOTE: For solutions in JavaScript, assume that there are 8 lines of input
 - (i.e. line 1 will be N and the next 7 lines will be the input lines
 -
 - Output sample:
 -
 - The 'N' longest lines, newline delimited. Do NOT print out empty lines.
 - Ignore all empty lines in the input. Ensure that there are no trailing empty
 - spaces on each line you print. Also ensure that the lines are printed out in
 - decreasing order of length i.e. the output should be sorted based on their
 - length e.g.
 -
 - San Francisco
 - Hello World
 -}

import Data.List (sortBy)
import Data.Ord (comparing)
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let (f:lns) = filter (\l -> length l > 0) $ lines txt
                       n = read f
                       lns' = reverse $ sortBy (comparing length) lns
                   in unlines $ take n lns'
