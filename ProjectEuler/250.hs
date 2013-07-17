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
    forM_ xs $ \v -> do
        cpy <- getAssocs arr
        forM_ cpy $ \(n,new) -> do
            let idx = (n+v) `rem` m
            cur <- readArray arr idx
            writeArray arr idx ((cur+new) `rem` a)
    return arr

solveProblem n m a = let xs = [ powerMod i i m | i <- [1..n] ]
                         arr = subsetSums xs m a
                     in (arr ! 0) - 1 -- leave out the empty subset

main = print $ solveProblem (250250) (250) (10^16)
