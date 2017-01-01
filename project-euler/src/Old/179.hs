-- 179.hs
{-
 - Find the number of integers 1 < n < 10^7, for which n and n + 1 have the same
 - number of positive divisors. For example, 14 has the positive divisors 1, 2,
 - 7, 14 while 15 has 1, 3, 5, 15.
 -}

{-
 - This is quite slow, but very beautiful. So called "executable specification".
 -}

import ProjectEuler.Prime (sigma)
import Data.List (group)


main = print $ solveProblem (10^7)
solveProblem bound = sum [ length g - 1 | g <- group $ map (sigma 0) [1..bound] ]
