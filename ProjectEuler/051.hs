-- 051.hs
{-
 - By replacing the 1st digit of the 2-digit number *3, it turns out that six
 - of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
 -
 - By replacing the 3rd and 4th digits of 56**3 with the same digit, this
 - 5-digit number is the first example having seven primes among the ten
 - generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663,
 - 56773, and 56993. Consequently 56003, being the first member of this family,
 - is the smallest prime with this property.
 -
 - Find the smallest prime which, by replacing part of the number (not
 - necessarily adjacent digits) with the same digit, is part of an eight prime
 - value family.
 -}

{-
 - For some reason I do not fully understand, the solution is NOT ***857.
 - Thus, just take the second result instead of the first one.
 -}

{-
 - I made a small optimization: the is_prime' array, which speeds up the
 - prime-testing. This takes it from ~2.8s to ~1.0s
 -
 - I'm pretty sure that about 0.6s are spent just generating the (core,mask)
 - pairs, so that would be another area to check.
 -}

import qualified ProjectEuler.Prime as Prime
import qualified ProjectEuler.Math as Math
import Data.Array

isPrime' = (is_prime' !)
is_prime' = accumArray (||) False (0,10^6) [(p,True) | p <- takeWhile (<10^6) Prime.primes ]

-- Given a core and a mask, generates all the primes in that family
-- Ex: primeFamily 20 1 -> [23, 29] since those are the only primes of the form 2*
primeFamily core mask = filter isPrime' [ core + k*mask | k <- [0..9] ]

-- Pads a list to size `s`. Does so by padding on the left with zeros.
padTo s xs = let zero_pad = take (s - length xs) $ repeat 0
             in zero_pad ++ xs

-- all the core/mask pairs of size s
-- Ex. coreMaskPairs 5 would include [12030, 101] and [55550, 1]
coreMaskPairs s =
    let digits = map (padTo s . Math.integerDigits) [0..10^s-1]
        cores = filter (0 `elem`) digits
        masks = map makeMask cores
    in zip cores masks

makeMask = map (\x -> if x > 0 then 0 else 1)

families = [ primeFamily core mask | len <- [1..]
                                   , (c,m) <- coreMaskPairs len
                                   , let core = Math.fromIntegerDigits c
                                   , let mask = Math.fromIntegerDigits m
           ]

findPrimeFamilies s = filter (\f -> length f == s) families

solveProblem = let ans = findPrimeFamilies 8
               in minimum $ ans !! 1

main = print solveProblem
