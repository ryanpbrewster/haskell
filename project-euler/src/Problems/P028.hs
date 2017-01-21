module Problems.P028
  ( solve
  ) where

{-
 - Starting with the number 1 and moving to the right in a clockwise direction
 - a 5 by 5 spiral is formed as follows:
 -
 - 21 22 23 24 25
 - 20  7  8  9 10
 - 19  6  1  2 11
 - 18  5  4  3 12
 - 17 16 15 14 13
 -
 - It can be verified that the sum of the numbers on the diagonals is 101.
 -
 - What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
 - formed in the same way?
 -}
{-
 - The downward diagonal looks like [1,3,7,13,21,...]
 - If you take the first difference, you see that it is
 -     [2, 4, 6, 8, ...] == [2,4..]
 - So we can reconstruct it via
 -     [1, 1+2, 1+2+4, 1+2+4+6, ...]
 -     scanl (+) 1 [2,4,6,8,...]
 -     scanl (+) 1 [2,4..]
 -
 - Likewise, the upward diagonal is [1,5,9,17,25,...]
 - The first difference is
 -     [4, 4, 8, 8, 12, 12, ...] == concat [[x,x] | x <- [4,8..] ]
 - Thus,
 -     scanl (+) 1 $ concat [[x,x] | x <- [4,8..]]
 -}
{-
 - Of course, this has a closed form.
 - The sum of the diagonals for the first `n` circles is:
 -     [1, 1+3+5+7+9, 1+3+5+7+9+13+17+21+25, ...]
 -     [1, 1+24, 1+24+76, 1+24+76+160, ...]
 -     1 + 2/3 n (13 + n (15 + 8 n)) /. n -> [1..]
 -}
solve :: String
solve = show $ solveProblem 1001

solveProblem :: Int -> Integer
solveProblem n = sum $ take (2 * n - 1) numberSpiralDiagonals

numberSpiralDiagonals :: [Integer]
numberSpiralDiagonals = scanl (+) 1 $ concatMap (replicate 4) [2,4 ..]

solveProblem'
  :: Integral a
  => a -> a
solveProblem' n = closedform $ (n - 1) `div` 2

closedform
  :: Integral a
  => a -> a
closedform n = 1 + 2 * n * (13 + n * (15 + 8 * n)) `div` 3
