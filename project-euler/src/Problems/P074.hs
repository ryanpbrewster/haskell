module Problems.P074
  ( solve
  ) where

{-
 - The number 145 is well known for the property that the sum of the factorial
 - of its digits is equal to 145:
 -
 - 1! + 4! + 5! = 1 + 24 + 120 = 145
 -
 - Perhaps less well known is 169, in that it produces the longest chain of
 - numbers that link back to 169; it turns out that there are only three such
 - loops that exist:
 -
 - 169 → 363601 → 1454 → 169 871 → 45361 → 871 872 → 45362 → 872
 -
 - It is not difficult to prove that EVERY starting number will eventually get
 - stuck in a loop. For example,
 -
 - 69 → 363600 → 1454 → 169 → 363601 (→ 1454) 78 → 45360 → 871 → 45361 (→ 871)
 - 540 → 145 (→ 145)
 -
 - Starting with 69 produces a chain of five non-repeating terms, but the
 - longest non-repeating chain with a starting number below one million is
 - sixty terms.
 -
 - How many chains, with a starting number below one million, contain exactly
 - sixty non-repeating terms?
 -}

import Data.Array
import Data.Maybe
import qualified Data.IntMap as M

solve :: String
solve = show $ solveProblem 60

-- We have too much hardcoded stuff in here, it's a bummer
bound = 1e6

solveProblem num_terms =
  length $ filter (== num_terms) $ map chainLength [1..bound]
  where
  chainLength = (memo !)
  memo = listArray (0, bound) $ map f [0..bound]
  f n = fromMaybe (g n) (M.lookup n hardcoded)
  g n = let n' = next n in 1 + if n' <= bound then memo ! n' else f n'

hardcoded =
  M.fromList $
    zip [0, 1, 2, 145, 40585]    (repeat 1) ++
    zip [871, 45361, 872, 45362] (repeat 2) ++
    zip [169, 363601, 1454]      (repeat 3)

next :: Int -> Int
next 0 = 0
next n =
  let (q, r) = n `divMod` 10
  in factorial r + next q

factorial = (factorials !)
  where
  factorials = listArray (0, 9) $ scanl (*) 1 [1..9]
