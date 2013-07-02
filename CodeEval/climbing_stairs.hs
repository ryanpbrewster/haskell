-- climbing_stairs.hs
{-
 - You are climbing a stair case. It takes n steps to reach to the top. Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each line in this file contains a positive integer which is the total number of stairs. e.g.
 -
 - 10
 - 20
 -
 - Output sample:
 -
 - Print out the number of ways to climb to the top of the staircase. e.g.
 -
 - 89
 - 10946
 -}

{-
 - Fibonacci. This is a hard problem?
 -}


import System.Environment (getArgs)

fibs = 1:1:zipWith (+) fibs (tail fibs)
main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = map read $ lines txt
                       anss = map (fibs !!) inputs
                   in unlines $ map show anss
