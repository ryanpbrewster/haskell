-- 067.hs
{-
 - By starting at the top of the triangle below and moving to adjacent numbers
 - on the row below, the maximum total from top to bottom is 23.
 -
 - 3
 - 7 4
 - 2 4 6
 - 8 5 9 3
 -
 - That is, 3 + 7 + 4 + 9 = 23.
 -
 - Find the maximum total from top to bottom in 067.in, a 15K text file
 - containing a triangle with one-hundred rows.
 -
 - NOTE: This is a much more difficult version of Problem 18. It is not
 - possible to try every route to solve this problem, as there are 299
 - altogether! If you could check one trillion (10^12) routes every second it
 - would take over twenty billion years to check them all. There is an
 - efficient algorithm to solve it. ;o)
 -}

import ProjectEuler.Util (rollBy)

-- addRows takes in two rows of length (x) and (x+1), and returns a
-- row of length (x+1)
addRows r1 r2 = let r1' = zipWith max ([0] ++ r1) (r1 ++ [0])
                in zipWith (+) r1' r2
solveProblem nums = maxTrianglePath $ rollBy [1..] nums
maxTrianglePath tri = maximum $ foldl1 addRows tri
main = do
    txt <- readFile "067.in"
    print $ solveProblem $ map read $ words txt





-- addRows' takes in two rows of length (x) and (x+1), and returns a
-- row of length (x).
addRows' r1 r2 = let r2' = zipWith max (init r2) (tail r2)
                 in zipWith (+) r1 r2'
maxTrianglePath' tri = head $ foldr1 addRows' tri
