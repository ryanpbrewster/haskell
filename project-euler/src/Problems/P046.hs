module Problems.P046
  ( solve
  ) where

{-
 - It was proposed by Christian Goldbach that every odd composite number can be
 - written as the sum of a prime and twice a square.
 -
 -     9  = 7  + 2*1^2
 -     15 = 7  + 2*2^2
 -     21 = 3  + 2*3^2
 -     25 = 7  + 2*3^2
 -     27 = 19 + 2*2^2
 -     33 = 31 + 2*1^2
 -
 - It turns out that the conjecture was false.
 -
 - What is the smallest odd composite that cannot be written as the sum of
 - a prime and twice a square?
 -}
import qualified Util.Prime as Prime

solve :: String
solve = show solveProblem

fitsConjecture n =
  any Prime.test $ takeWhile (> 0) [n - 2 * i ^ 2 | i <- [0 ..]]

solveProblem =
  let odd_composites = filter (not . Prime.test) [9,11 ..]
  in head $ filter (not . fitsConjecture) odd_composites
