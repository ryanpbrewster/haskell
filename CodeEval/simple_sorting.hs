-- simple_sorting.hs
{-
 - Write a program which sorts numbers.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Input
 - example is the following
 -
 - 70.920 -38.797 14.354 99.323 90.374 7.581
 - -37.507 -3.263 40.079 27.999 65.213 -55.552
 - Output sample:
 -
 - Print sorted numbers in the following way.
 -
 - -38.797 7.581 14.354 70.920 90.374 99.323
 - -55.552 -37.507 -3.263 27.999 40.079 65.213
 -}


import System.Environment (getArgs)
import Data.List (intercalate, partition)
import Text.Printf (printf)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

atof :: String -> Double
atof = read

solveProblem txt = let inputs = [ map atof $ words ln | ln <- lines txt ]
                       anss = map mySort inputs
                       outputs = [[printf "%.3f" x :: String | x <- xs] | xs <- anss]
                   in unlines [ intercalate " " ss | ss <- outputs ]

mySort = myMergesort

myQuicksort [] = []
myQuicksort (x:xs) = let (lo,hi) = partition (<x) xs
                     in (myQuicksort lo) ++ [x] ++ (myQuicksort hi)

myMergesort [] = []
myMergesort [x] = [x]
myMergesort xs = let (a,b) = splitAt (length xs `quot` 2) xs
                 in merge (myMergesort a) (myMergesort b)

merge [] ys = ys
merge xs [] = xs
merge xs@(x:xx') ys@(y:yy') | x < y  = x:merge xx' ys
                            | x == y = x:y:merge xx' yy'
                            | x > y  = y:merge xs yy'
