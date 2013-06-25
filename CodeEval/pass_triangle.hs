-- pass_triangle.hs
{-
 - By starting at the top of the triangle and moving to adjacent numbers on the row below, the maximum total from top to bottom is 27.
 - 
 -    5
 -   9 6
 -  4 6 8
 - 0 7 1 5
 - 
 - 5 + 9 + 6 + 7 = 27
 - 
 - Input sample:
 - 
 - Your program should accept as its first argument a path to a filename. Input example is the following
 - 
 - 5
 - 9 6
 - 4 6 8
 - 0 7 1 5
 - 
 - You make also check full input file which will be used for your code evaluation.
 - 
 - Output sample:
 - 
 - The correct output is the maximum sum for the triangle. So for the given example the correct answer would be
 - 
 - 27
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let tri = [ map read $ words ln | ln <- lines txt ]
                       ans = maxTriangleSum tri
                   in show ans

maxTriangleSum tri = head $ foldr1 joinRows tri
    where joinRows r1 r2 = let r2_maxs = zipWith max (init r2) (tail r2)
                           in zipWith (+) r1 r2_maxs
