module Problems.P240
  ( solve
  ) where

{-
 - There are 1111 ways in which five 6-sided dice (sides numbered 1 to 6) can
 - be rolled so that the top three sum to 15. Some examples are:
 -
 - D1,D2,D3,D4,D5 = 4,3,6,3,5
 - D1,D2,D3,D4,D5 = 4,3,3,5,6
 - D1,D2,D3,D4,D5 = 3,3,3,6,6
 - D1,D2,D3,D4,D5 = 6,6,3,3,3
 -
 - In how many ways can twenty 12-sided dice (sides numbered 1 to 12) be rolled
 - so that the top ten sum to 70?
 -}
{-
 - There are <n> dice. You take the top <k> rolls, which must sum to <t>.
 - The dice each go from 1 to <d>.
 -
 - This is the basic idea: You need to make <t> out of <k> dice. So make those
 - first, using <partitions t k [1..d]>. It will be helpful to work with an example,
 - so consider the example above, with t = 15, k = 3, d = 6.
 - 
 - The partitions are [6,6,3], [6,5,4], and [5,5,5]
 - For [6,6,3], we have a whole class of solutions:
 -     [6,6,3,x,x], where x < 3
 -     [6,6,3,3,x]
 -     [6,6,3,3,3]
 - In any case, we have a ``restricted'' set (from the top k dice, and any repeats),
 - and a ``free'' set (from the x < 3). We are going to place the restricted dice,
 - then the free dice.
 -
 - For [6,6,3,x,x], the restricted set is [6,6,3]
 - There are Binomial[5,3] ways to place those dice in the set of 5 rolls,
 - and 3!/(2! * 1!) ways to shuffle them. The 2 remaining dice are ``free'',
 - in the sense that you can pick whatever you'd like, as long as they're <3.
 - Thus, there are 2^2 ways to pick them.
 -
 - [6,6,3,3,x] = Binomial[5,4] * 4!/(2! * 2!) * 2^1
 - [5,5,5,x,x] = Binomial[5,3] * 3!/(3!) * 4^2
 - etc.
 -
 - ways top rep n calculates this
 - ways [6,6,3] 1 5 == Binomial[5,4] * 4!/(2! * 2!) * 2^1 == 60
 -}
import Data.List (sort, group)
import Util.List (tuples, ordTuples)
import Util.Math (binomial)

solve :: String
solve = "unsolved"
-- solve = show $ solveProblem 20 10 70 12

-- find all the partitions of n into exactly k elements of xs (repeats allowed)
-- xs must be all positive integers
partitions n k xs = partitions' n k (reverse $ sort xs)

partitions' 0 0 xs = [[]]
partitions' n 0 xs = []
partitions' n k [] = []
partitions' n k (x:xs)
  | k * x < n = []
  | otherwise =
    let with = partitions' (n - x) (k - 1) (x : xs)
        without = partitions' n k xs
    in (map (x :) with) ++ without

fact n = product [1 .. n]

ways :: [Integer] -> Integer -> Integer -> Integer
ways top rep n =
  let m = last top
      top' = top ++ replicate (fromInteger rep) m
      occ = fromIntegral $ length top'
      unocc = n - occ
      fact_lens = map (fromIntegral . fact . length) $ group top'
      shuffles = fact occ `div` product fact_lens
      free_choices = (m - 1) ^ unocc
  in (binomial n occ) * shuffles * free_choices

-- I refer to "counts", because they ignore order.
-- For instance, given a count of [3,3,2,1], the actual roll could be
--     [1,2,3,3], [1,3,2,3], [1,3,3,2], [2,1,3,3], [2,3,1,3], [2,3,3,1], etc.
solveProblem :: Integer -> Integer -> Integer -> Integer -> Integer
solveProblem n k t d =
  let good_tops = partitions t k [1 .. d] -- find the good top dice rolls
  in sum [ways top rep n | top <- good_tops, rep <- [0 .. n - k]]
