module Problems.P249
  ( solve
  ) where

{-
 - Let S = {2, 3, 5, ..., 4999} be the set of prime numbers less than 5000.
 -
 - Find the number of subsets of S, the sum of whose elements is a prime number.
 - Enter the rightmost 16 digits as your answer.
 -}
import qualified Util.Prime as Prime

import Data.Array.ST
import Data.Array.Unboxed

import Control.Monad
import Control.Monad.ST

solve :: String
solve = "unsolved"
-- solve = show solveProblem

d = 16

primes = map fromInteger Prime.primes

solveProblem = primeSubsetSums (takeWhile (< 5000) primes)

primeSubsetSums :: [Int] -> Integer
primeSubsetSums xs =
  let arr = subsetSumCount xs
      (_, m) = bounds arr
      counts = [arr ! p | p <- takeWhile (<= m) primes]
  in (sum $ map fromIntegral counts) `mod` (10 ^ d)

subsetSumCount :: [Int] -> UArray Int Int
subsetSumCount xs =
  runSTUArray $ do
    let n = length xs
        cumsums = listArray (0, n) $ scanl (+) 0 xs :: UArray Int Int
    arr <- newArray (0, cumsums ! n) 0
    writeArray arr 0 1
    forM_ (zip [1 ..] xs) $ \(vi, v) -> do
      let i_hi = cumsums ! vi
      forM_ [i_hi,i_hi - 1 .. v] $ \i -> do
        cur <- readArray arr i
        prev <- readArray arr (i - v)
        writeArray arr i ((prev + cur) `mod` (10 ^ d))
    return arr
