-- 250.hs
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

import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import ProjectEuler.Math (powerMod)
import Data.Int

-- Return a list, ss, where ss !! k is the number of subsets, s, of
-- of xs where sum(s) == k (mod m)
subsetSums xs m a = runSTUArray $ do
    arr <- newArray (0,m-1) 0 :: ST s (STUArray s Int Int64)
    writeArray arr 0 1
    forM_ xs $ \i -> do
        let v = powerMod i i m
        cpy <- newArray (0,m-1) 0 :: ST s (STUArray s Int Int64)
        forM_ [0..m-1] $ \n -> do
            e <- readArray arr n
            writeArray cpy n e
        forM_ [0..m-1] $ \n -> do
            new <- readArray cpy ((n-v) `mod` m)
            cur <- readArray arr n
            writeArray arr n ((cur+new) `mod` a)
    return arr

solveProblem n m a = let arr = subsetSums [1..n] m a
                     in (arr ! 0) - 1 -- leave out the empty subset

main = print $ solveProblem (250250) (250) (10^16)
