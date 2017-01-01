-- 126.hs
{-
 -
 -
 - The minimum number of cubes to cover every visible face on a cuboid
 - measuring 3 x 2 x 1 is twenty-two.
 -
 - If we then add a second layer to this solid it would require forty-six cubes
 - to cover every visible face, the third layer would require seventy-eight
 - cubes, and the fourth layer would require one-hundred and eighteen cubes to
 - cover every visible face.
 -
 - However, the first layer on a cuboid measuring 5 x 1 x 1 also requires
 - twenty-two cubes; similarly the first layer on cuboids measuring 5 x 3 x 1,
 - 7 x 2 x 1, and 11 x 1 x 1 all contain forty-six cubes.
 -
 - We shall define C(n) to represent the number of cuboids that contain n cubes
 - in one of its layers. So C(22) = 2, C(46) = 4, C(78) = 5, and C(118) = 8.
 -
 - It turns out that 154 is the least value of n for which C(n) = 10.
 -
 - Find the least value of n for which C(n) = 1000.
 -}

{-
 - I derived
 -     count (l,w,h) n = 2*(l*w + l*h + w*h) + 4*(n-1)*(l+w+h) + 4*(n-2)*(n-1)
 - at some point. See $HOME/prog/python/project_euler/126_visualizer.py
 -}

import Data.List (group)
import ProjectEuler.Util (merge, mergeInf)

main = print solveProblem

solveProblem = fst $ head $ filter (\(n,c) -> c == 100) layerCounts

cubeCount (l,w,h) n = 2*(l*w + l*h + w*h) + 4*(n-1)*(l+w+h) + 4*(n-2)*(n-1)

cubeLayers cuboid@(l,w,h) = takeWhile (<20000) [ cubeCount cuboid n | n <- [1..] ]

layersByMaxDim l = foldl merge [] [ cubeLayers (l,w,h) | w <- [1..l], h <- [1..w] ]

layerCounts = let all_layers = mergeInf [ layersByMaxDim l | l <- [1..] ]
              in [ (head g, length g) | g <- group all_layers ]
