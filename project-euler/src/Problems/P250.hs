module Problems.P250
  ( solve
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST

{-
 - Find the number of non-empty subsets of {1^1, 2^2, 3^3,..., 250250^250250},
 - the sum of whose elements is divisible by 250. Enter the rightmost 16 digits
 - as your answer.
 -}
{-
 - So let m = 250
 - We only care about the subset sums (mod m), so we can just keep track of them.
 - For any value, v (such as 250^250),
 -    ways[n] += ways[(n-v) `mod` m]
 -}
{-
 - x^x (mod n) is periodic
 -
 - In general, x^(phi n) = 1 (mod n)
 -             n = 0 (mod n)
 - So
 -             (x+k)^(x+k) = x^x (mod n)
 -      where k = LCM[n, phi n]
 - Thus, we don't really need to compute all the powerMods
 - We can just repeat after a certain period, which is lcm n (phi n)
 -}
import Data.Array.Unboxed
import Data.Int

import Util.Math (powerMod)
import Util.Prime (phi)

solve :: String
solve = show $ solveProblem (250250) (250) (10 ^ 16)

-- Return a list, ss, where ss !! k is the number of subsets, s, of
-- of xs where sum(s) == k (mod m)
subsetSums :: [Int] -> Int -> Int64 -> UArray Int Int64
subsetSums xs m a =
  runSTUArray $ do
    arr <- newArray (0, m - 1) 0 :: ST s (STUArray s Int Int64)
    writeArray arr 0 1
    forM_ xs $ \v -> do
      cpy <- getAssocs arr
      forM_ cpy $ \(n, new) -> do
        let idx = (n + v) `mod` m
        cur <- readArray arr idx
        writeArray arr idx ((cur + new) `mod` a)
    return arr

solveProblem :: Int -> Int -> Int64 -> Int64
solveProblem n m a =
  let period = lcm m (fromIntegral $ phi $ fromIntegral m)
      xs = cycle [powerMod i i m | i <- [1 .. period]]
      arr = subsetSums (take n xs) m a
  in (arr ! 0) - 1 -- leave out the empty subset
