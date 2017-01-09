module Problems.P050
  ( solve
  ) where

-- 050.hs
{-
 - The prime 41, can be written as the sum of six consecutive primes:
 -     41 = 2 + 3 + 5 + 7 + 11 + 13
 -
 - This is the longest sum of consecutive primes that adds to a prime below
 - one-hundred.
 -
 - The longest sum of consecutive primes below one-thousand that adds to
 - a prime, contains 21 terms, and is equal to 953.
 -
 - Which prime, below one-million, can be written as the sum of the most
 - consecutive primes?
 -}
import qualified Util.Prime as Prime

solve :: String
solve = show $ solveProblem 1e6

roll _ [] = []
roll k xs = (take k xs) : (roll k $ tail xs)

-- elementsRequired xs targ
-- How many elements from xs are required before we sum to targ?
-- Should satisfy:
--     sum $ take (elementsRequired xs targ) xs >= targ
-- as long as xs is infinite (and positive)
elementsRequired (x:xs) targ
  | targ <= 0 = 0
  | otherwise = 1 + elementsRequired xs (targ - x)

solveProblem bound =
  let primes = takeWhile (< bound) Prime.primes
      len = elementsRequired primes bound
      sols = concat [findSolutions k primes bound | k <- [len,len - 1 .. 1]]
  in sum $ head sols

findSolutions k primes bound =
  let candidates = takeWhile (\ps -> sum ps < bound) (roll k primes)
  in filter (Prime.test . sum) candidates
