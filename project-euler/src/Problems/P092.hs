module Problems.P092
  ( solve
  , bruteForceSolve
  , memoizedSolve
  , numDigitsSolve
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
import Data.Map (Map)
import qualified Data.Map as M

solve :: String
solve = show $ numDigitsSolve 7

numDigitsSolve num_digits =
   let counts = sumCounts num_digits $ map (^2) [0..9]
   in sum [ v | (k, v) <- M.toList counts, squareChainTerminator k == 89 ]

sumCounts :: Int -> [Int] -> Map Int Int
sumCounts 0 _ = M.singleton 0 1
sumCounts n xs =
   let counts = sumCounts (n-1) xs
   in M.fromListWith (+) [ (k + x, v) | x <- xs, (k, v) <- M.toList counts ]

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

squareChainTerminator 0 = 0
squareChainTerminator 1 = 1
squareChainTerminator 89 = 89
squareChainTerminator n = squareChainTerminator $ next n

next :: Int -> Int
next n = fromIntegral $ sum $ map (^2) $ integerDigits $ fromIntegral n
