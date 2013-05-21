-- Frog.hs
{-
 - You have a garden that is W x H
 - Frogs jumped through it at night, landing at points (x_i, y_i)
 - Given that any given frog has a set jump vector, what is the maximum
 - number of landings that any frog could have made in your garden?
 -}

{-
 - The general strategy is as follows:
 - For any pair of points, p1 and p2, there is a defined frog path that
 - contains both p1 and p2. For instance, in a 5x8 box, the points
 -     (2,3) and (3,2) define the frog path
 -
 -     ...o....
 -     ..x.....
 -     .x......
 -     o.......
 -     ........
 -
 -  The 'o's there represent extrapolated points. Thus, for all pairs of points
 -  provided, construct the frog path and see if all the extrapolated points are
 -  also in the observed set of points. Find the biggest one of those paths and
 -  you're on your way.
 -}

{-
 - Note: This runs in O(n^3) time, but can be easily sped up to O(n^2) time.
 - Suppose we are trying to see if a frog path containing (p1,p2) is legit.
 - We will compute [p3,p4,...] and see if all of those are in the point list.
 - However, observe that this frog path is legit if and only if [p2,p3,...] is legit.
 - So if we traverse the pair of points in order, we can always see in O(1) time
 - whether a set of points defines a legit frog path.
 -}

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

legitFrogPath fp points = and [ p `elem` points | p <- fp ]

longestFrogPath w h points = let frog_paths = [ frogPath p1 p2 w h | (p1,p2) <- pairs points ]
                                 legit = (\fp -> legitFrogPath fp points)
                                 legit_frog_paths = filter legit frog_paths
                             in maximum $ map length $ legit_frog_paths
