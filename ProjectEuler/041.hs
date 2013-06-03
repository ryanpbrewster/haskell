-- 041.hs
{-
 - We shall say that an n-digit number is pandigital if it makes use of all the
 - digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
 - also prime.
 -
 - What is the largest n-digit pandigital prime that exists?
 -}

{-
 - Some things to note:
 - If the sum of the digits is a multiple of 3 then the number is divisible by 3.
 - Thus, there are no 5, 6, 8, or 9-digit pandigital primes, since
 -     sum [1..8] == 36
 -     sum [1..9] == 45
 -
 - Thus, we're probably looking for a 7-digit pandigital prime.
 -}

import Data.List (permutations)
import qualified ProjectEuler.Math as Math
import qualified ProjectEuler.Prime as Prime

solveProblem = let possibilities = map Math.fromIntegerDigits $ permutations [1..7]
               in maximum $ filter Prime.test possibilities

main = print solveProblem
