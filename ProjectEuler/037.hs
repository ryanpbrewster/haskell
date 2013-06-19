-- 037.hs
{-
 - The number 3797 has an interesting property. Being prime itself, it is
 - possible to continuously remove digits from left to right, and remain prime
 - at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
 - left: 3797, 379, 37, and 3.
 -
 - Find the sum of the only eleven primes that are both truncatable from left
 - to right and right to left.
 -
 - NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
 -}

{-
 - Small optimization: a truncatable prime must end in {3, 7}.
 - {0, 2, 4, 5, 6, 8} all directly mean that a number is divisible by 2 or 5
 - {1, 9} are not prime, so truncating will not work
 -}

import Data.List (inits, tails)
import qualified ProjectEuler.Prime as Prime
import qualified ProjectEuler.Math as Math

main = print solveProblem

solveProblem = let possibles = concat [ [p+3, p+7] | p <- [10,20..] ]
                   truncatable_primes = take 11 $ filter isTruncatable possibles
               in sum truncatable_primes

isTruncatable n = all Prime.bfTest $ (leftTruncations n) ++ (rightTruncations n)

leftTruncations n  = map Math.fromIntegerDigits $ tail $ inits $ Math.integerDigits n
rightTruncations n = map Math.fromIntegerDigits $ init $ tails $ Math.integerDigits n
