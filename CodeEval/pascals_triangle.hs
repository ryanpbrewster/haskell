-- pascals_triangle.hs
{-
 - A pascals triangle row is contructed by looking at the previous row and
 - adding the numbers to its left and right to arrive at the new value. If
 - either the number to its left/right is not present, substitute a zero in
 - it's place.
 -                 1
 -               1   1
 -             1   2   1
 -            1  3   3   1
 -          1  4   6   4   1
 -         1  5  10  10  5   1
 - Input sample:
 - 
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains a positive integer which indicates the depth of
 - the triangle (1 based). e.g.
 - 
 - 6
 - Output sample:
 - 
 - Print out the resulting pascal triangle upto the requested depth in row major form e.g.
 - 
 - 1 1 1 1 2 1 1 3 3 1 1 4 6 4 1 1 5 10 10 5 1
 -}


import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = map read $ lines txt
                       anss = [ concat $ take n pascals_triangle | n <- inputs ]
                       outputs = [ unwords $ map show ans | ans <- anss ]
                   in unlines outputs

pascals_triangle = iterate nextRow [1]
    where nextRow r = zipWith (+) ([0]++r) (r++[0])
