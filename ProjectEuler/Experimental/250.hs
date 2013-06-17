-- 250.hs
{-
 - Find the number of non-empty subsets of {1^1, 2^2, 3^3,..., 250250^250250},
 - the sum of whose elements is divisible by 250. Enter the rightmost 16 digits
 - as your answer.
 -}

{-
 - So let m = 250
 - We only care about the subset sums (mod m), so we can just keep track of them.
 - For any value, v (such as 250^250),
 -    ways[n] += ways[(n-v) `mod` m]
 -}

import Data.List
import ProjectEuler.Math (powerMod)

shift n xs = let (left,right) = splitAt n xs
             in right ++ left

-- Return a list, ss, where ss !! k is the number of subsets, s, of
-- of xs where sum(s) == k (mod m)
subsetSums xs m a = foldl' f (1:replicate (m-1) 0) xs
    where f ss (b,e) = let v = powerMod (b `mod` m) e m
                       --in map (`mod` a) $ zipWith (+) ss (drop v $ cycle ss)
                       in map (`mod` a) $ zipWith (+) ss (shift v ss)

solveProblem up m a = let xs = [(i,i) | i <- [1..up]]
                          ss = subsetSums xs m a
                      in (head ss) - 1


main = print $ solveProblem (50000) (250) (10^16)
