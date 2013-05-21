-- Frog_set.hs
-- Same as Frog.hs, except it uses Data.Set to speed up member lookups

import qualified Data.Set as DS

atoi :: String -> Int
atoi = read

main = do
    input <- getContents
    print (solveProblem $ map atoi (words input))


solveProblem :: [Int] -> Int
solveProblem (w:h:n:xs) = let points = roll xs 2
                          in longestFrogPath w h points

-- roll [1,2,3,4,5,6] 2 = [ [1,2], [3,4], [5,6] ]
roll [] _ = []
roll xs n = let (front,back) = splitAt n xs
            in front:(roll back n)

pairs [] = []
pairs (x:xs) = [ (x,y) | y <- xs ] ++ (pairs xs)


frogPath p1 p2 w h = let [x1,y1] = p1
                         [x2,y2] = p2
                         dx = x2-x1
                         dy = y2-y1
                         inBounds [x,y] = 1 <= x && x <= w && 1 <= y && y <= h
                         p1_p2_etc = takeWhile inBounds [ [x2+i*dx, y2+i*dy] | i <- [1..] ]
                         etc_p1_p2 = takeWhile inBounds [ [x1-i*dx, y1-i*dy] | i <- [1..] ]
                     in [p1,p2] ++ p1_p2_etc ++ etc_p1_p2

legitFrogPath fp points = and [ p `DS.member` points | p <- fp ]

longestFrogPath w h points = let frog_paths = [ frogPath p1 p2 w h | (p1,p2) <- pairs points ]
                                 point_set = DS.fromList points
                                 legit = (\fp -> legitFrogPath fp point_set)
                                 legit_frog_paths = filter legit frog_paths
                             in maximum $ map length $ legit_frog_paths
