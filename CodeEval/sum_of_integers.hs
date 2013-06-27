-- sum_of_integers.hs
{-
 - Write a program to determine the largest sum of contiguous integers in
 - a list.
 - Input sample:
 -
 - The first argument will be a text file containing a comma separated list of
 - integers, one per line. e.g.
 -
 - -10, 2, 3, -2, 0, 5, -15
 - 2,3,-2,-1,10
 -
 - Output sample:
 -
 - Print to stdout, the largest sum. In other words, of all the possible
 - contiguous subarrays for a given array, find the one with the largest sum,
 - and print that sum.
 - e.g.
 -
 - 8
 - 12
 -}


import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = [ map read $ wordsBy " ," ln | ln <- lines txt ]
                       anss = map maxSubarraySum inputs
                   in unlines $ map show anss

maxSubarraySum xs = maximum $ scanl (\x y -> y + max x 0) 0 xs

wordsBy delims s = wordsBy' delims s
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)
