-- number_pairs.hs
{-
 - You are given a sorted array of positive integers and a number 'X'. Print
 - out all pairs of numbers whose sum is equal to X. Print out only unique
 - pairs and the pairs should be in ascending order
 - Input sample:
 -
 - Your program should accept as its first argument a filename. This file will
 - contain a comma separated list of sorted numbers and then the sum 'X',
 - separated by semicolon. Ignore all empty lines. If no pair exists, print the
 - string NULL eg.
 -
 - 1,2,3,4,6;5
 - 2,4,5,6,9,11,15;20
 - 1,2,3,4;50
 -
 - Output sample:
 -
 - Print out the pairs of numbers that equal to the sum X. The pairs should
 - themselves be printed in sorted order i.e the first number of each pair
 - should be in ascending order .e.g.
 -
 - 1,4;2,3
 - 5,15;9,11
 - NULL
 -}


import Data.List (intercalate, sort)
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

pairsThatSumTo targ [] = []
pairsThatSumTo targ (x:xs) = let useit = [ (x,y) | y <- xs, x+y == targ ]
                                 loseit = pairsThatSumTo targ xs
                             in useit ++ loseit

showPairs [] = "NULL"
showPairs prs = let prs' = [ (show p) ++ "," ++ (show q) | (p,q) <- prs ]
                in intercalate ";" prs'

solveProblem txt = let inputs = [ map read $ wordsBy ",;" ln | ln <- lines txt ]
                       anss = [ pairsThatSumTo (last inp) (init inp) | inp <- inputs ]
                       outputs = map (showPairs.sort) anss
                   in unlines outputs

wordsBy delims s = wordsBy' delims s
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)
