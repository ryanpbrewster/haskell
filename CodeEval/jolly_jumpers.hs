-- jolly_jumpers.hs
{-
 - Credits: Programming Challenges by Steven S. Skiena and Miguel A. Revilla
 -
 - A sequence of n > 0 integers is called a jolly jumper if the absolute values
 - of the differences between successive elements take on all possible values
 - 1 through n - 1. eg.
 -
 - 1 4 2 3
 -
 - is a jolly jumper, because the absolute differences are 3, 2, and 1,
 - respectively. The definition implies that any sequence of a single integer
 - is a jolly jumper. Write a program to determine whether each of a number of
 - sequences is a jolly jumper.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file is one test case. Each test case will contain an integer
 - n < 3000 followed by n integers representing the sequence. The integers are
 - space delimited.
 -
 - Output sample:
 -
 - For each line of input generate a line of output saying 'Jolly' or 'Not
 - jolly'.
 -}


import System.Environment (getArgs)
import Data.List (sort)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

output b = if b then "Jolly" else "Not jolly"
solveProblem txt = let inputs = [ map read $ tail $ words ln | ln <- lines txt ]
                       anss = map isJollyJumper inputs
                       outputs = map output anss
                   in unlines outputs

diffs xs = zipWith (-) (init xs) (tail xs)
isJollyJumper xs = let n = length xs
                   in (sort $ map abs $ diffs xs) == [1..n-1]
