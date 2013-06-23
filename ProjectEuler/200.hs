-- 200.hs
{-
 - We shall define a sqube to be a number of the form, p^2q^3, where p and
 - q are distinct primes.  For example, 200 = 5^2 2^3 or 120072949 = 23^2 61^3.
 -
 - The first five squbes are 72, 108, 200, 392, and 500.
 -
 - Interestingly, 200 is also the first number for which you cannot change any
 - single digit to make a prime; we shall call such numbers, prime-proof. The
 - next prime-proof sqube which contains the contiguous sub-string "200" is
 - 1992008.
 -
 - Find the 200th prime-proof sqube containing the contiguous sub-string "200".
 -}

import ProjectEuler.Util (mergeInf)
import ProjectEuler.Prime (primes, test)
import Data.List (isInfixOf)

squbes = mergeInf [ [p^2 * q^3 | p <- primes] | q <- primes ]

digitPlaces n = digitPlaces' n 1
    where digitPlaces' 0 _ = []
          digitPlaces' n acc = let (q,r) = n `divMod` 10
                               in (r,acc) : digitPlaces' q (10*acc)

isPrimeProof n = let ds = digitPlaces n
                     all_changes = [ n + k*dp | (d,dp) <- ds, k <- [-d..9-d] ]
                 in not $ any test all_changes

solveProblem k =
    let candidates = filter (\n -> "200" `isInfixOf` (show n)) squbes
        legits = filter isPrimeProof candidates
    in legits !! k

main = print $ solveProblem 199
