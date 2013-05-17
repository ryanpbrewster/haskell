-- 010.hs
{-
 - The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 - Find the sum of all the primes below two million.
 -}

import qualified ProjectEuler.Prime as Prime

solveProblem bound = sum $ takeWhile (<bound) Prime.primes

main = print $ solveProblem $ floor 2e6
