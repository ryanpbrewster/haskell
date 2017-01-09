module Problems.P119
  ( solve
  ) where

{-
 - The number 512 is interesting because it is equal to the sum of its digits
 - raised to some power: 5 + 1 + 2 = 8, and 83 = 512. Another example of
 - a number with this property is 614656 = 284.
 -
 - We shall define an to be the nth term of this sequence and insist that
 - a number must contain at least two digits to have a sum.
 -
 - You are given that a2 = 512 and a10 = 614656.
 -
 - Find a30.
 -}
import Util.List (mergeInf)
import Util.Math (integerDigits)

solve :: String
solve = show $ solveProblem 29

solveProblem k = ans !! k

ans =
  let candidates = mergeInf [powersOf n | n <- [2 ..]]
      sols = [v | (v, b) <- candidates, sum (integerDigits v) == b]
  in dropWhile (< 10) sols

-- <n> can only be a digit-sum of large enough numbers
-- For instance, 28 will not be a legitimate digit-sum until we get to 4-digit
-- numbers (since 999 -> 27 < 28)
powersOf n =
  let powers = iterate (n *) n
                 -- 9*d == n (d is the number of digits)
                 -- 10^d <= n^kmin
                 -- d*Log[10]/Log[n] <= kmin
      n' = fromIntegral n
      min_exponent = round $ (n' / 9) * log 10 / log n'
  in zip (drop min_exponent powers) (repeat n)
