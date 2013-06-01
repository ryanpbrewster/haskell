-- 035.hs
{-
 - The number, 197, is called a circular prime because all rotations of the
 - digits: 197, 971, and 719, are themselves prime.
 -
 - There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
 - 71, 73, 79, and 97.
 -
 - How many circular primes are there below one million?
 -}

import Data.List (tails, inits)
import qualified ProjectEuler.Prime as Prime
import qualified ProjectEuler.Math as Math


rotations xs = zipWith (++) (tails xs) (inits xs)

good digit = digit `elem` [1,3,7,9]

isCircularPrime 2 = True
isCircularPrime 5 = True
isCircularPrime p = let digits = Math.integerDigits p
                        rots   = map Math.fromIntegerDigits $ rotations digits
                    in (all good digits) && (all Prime.test rots)

solveProblem = generalProblem (10^6)

generalProblem bound = let low_primes = takeWhile (<bound) Prime.primes
                       in length $ filter isCircularPrime low_primes

main = print solveProblem
