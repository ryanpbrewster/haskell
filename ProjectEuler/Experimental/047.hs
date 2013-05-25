-- 047.hs
{-
 - The first two consecutive numbers to have two prime factors each, with all four distinct, are:
 -     14 = 2*7
 -     15 = 3*5
 -
 - The first three consecutive numbers to have three prime factors each, where no adjacent number
 - share a prime factor, are:
 -     644 = 2^2 * 7 * 23
 -     645 = 3 * 5 * 43
 -     646 = 2 * 17 * 19
 -
 - Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?
 -}

import qualified ProjectEuler.Prime as Prime
import qualified Data.Set as DS

list_factors = [ (n, DS.fromList $ Prime.factors n) | n <- [2..] ]
num_factors = map (DS.size.snd) list_factors


-- consecutiveDistinct k factors num_factors
consecutiveDistinct k lofs nofs =
    if (all (==k) $ take k nofs) && (allDistinct $ take k lofs) then
        take k lofs
    else
        consecutiveDistinct k (tail lofs) (tail nofs)

allDistinct lofs = let factors = map snd lofs
                       pair_overlaps = zipWith DS.intersection (init factors) (tail factors)
                   in all DS.null pair_overlaps

solveProblem = fst $ head $ consecutiveDistinct 4 list_factors num_factors

main = print solveProblem
