module Problems.P092
  ( solve
  , bruteForceSolve
  , memoizedSolve
  ) where

{-
 - A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

 - For example,

 - 44 → 32 → 13 → 10 → 1 → 1
 - 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89

 - Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

 - How many starting numbers below ten million will arrive at 89?
 -}

import Data.Array
import Util.Math (integerDigits)

solve :: String
solve = show $ memoizedSolve 10e6

memoizedSolve :: Int -> Int
memoizedSolve bound = length $ filter (== 89) $ map (memo !) [1..bound]
  where
    memo = listArray (1, bound) $ map f [1..bound]
    f 1 = 1
    f 89 = 89
    f n =
      let n' = next n
      in if n' <= bound then memo ! n' else f n'

bruteForceSolve :: Int -> Int
bruteForceSolve bound = length $ filter (== 89) $ map squareChainTerminator [1..bound]
  where
  squareChainTerminator 1 = 1
  squareChainTerminator 89 = 89
  squareChainTerminator n = squareChainTerminator $ next n

next :: Int -> Int
next n = fromIntegral $ sum $ map (^2) $ integerDigits $ fromIntegral n
