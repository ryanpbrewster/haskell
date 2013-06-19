-- 347.hs
{-
 - The largest integer ≤ 100 that is only divisible by both the primes 2 and
 - 3 is 96, as 96=32*3=25*3. For two distinct primes p and q let M(p,q,N) be
 - the largest positive integer ≤N only divisible by both p and q and
 - M(p,q,N)=0 if such a positive integer does not exist.
 - 
 - E.g. M(2,3,100)=96.
 - M(3,5,100)=75 and not 90 because 90 is divisible by 2 ,3 and 5.
 -
 - Also M(2,73,100)=0 because there does not exist a positive integer ≤ 100
 - that is divisible by both 2 and 73.
 -
 - Let S(N) be the sum of all distinct M(p,q,N). S(100)=2262.
 -
 - Find S(10 000 000).
 -}

import qualified ProjectEuler.Prime as Prime
import qualified ProjectEuler.Util as Util
import qualified Data.Set as DS
import Data.List (tails)

allMultiples [] _ = [1]
allMultiples (p:ps) bound = let xs = allMultiples ps bound
                                p_powers = takeWhile (<=bound) $ iterate (p*) p
                                xs_with_p = [ p' * x | p' <- p_powers, x <- xs ]
                            in filter (<= bound) xs_with_p

primePairs n = [ (p,q) | ps <- takeWhile (\(p:ps) -> p^2 < n) (tails Prime.primes)
                       , let p = head ps
                       , q <- takeWhile (\q -> p*q < n) (tail ps) ]

fm p q n = maximum $ allMultiples [p,q] n

fs n = DS.fromList $ [ fm p q n | (p,q) <- primePairs n ]

solveProblem n = DS.foldl (+) 0 (fs n)

main = print $ solveProblem (10^7)
