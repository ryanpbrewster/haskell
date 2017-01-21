module Problems.P014
  ( solve
  ) where

{-
 - Project Euler problem 14
 - Find the number, 1 <= k <= n, such that the Collatz sequence seeded by k is longest
 -}
import Data.Array
import Util.List (maximumBy)

solve :: String
solve = show $ fastSolve 1e6

slowSolve :: Int -> Int
slowSolve bound = maximumBy collatz [1 .. bound]

fastSolve :: Int -> Int
fastSolve bound = maximumBy (memo !) [1 .. bound]
  where
    memo = listArray (1, bound) (1 : map f [2 .. bound])
    f n =
      let n' = next n
      in 1 +
         if n' <= bound
           then memo ! n'
           else f n'

collatz :: Int -> Int
collatz 1 = 1
collatz n = 1 + collatz (next n)

next :: Int -> Int
next n =
  if even n
    then n `div` 2
    else 3 * n + 1
