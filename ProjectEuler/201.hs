-- 201.hs
{-
 - For any set A of numbers, let sum(A) be the sum of the elements of A.
 - Consider the set B = {1,3,6,8,10,11}.
 - There are 20 subsets of B containing three elements, and their sums are:
 -
 - sum({1,3,6}) = 10,
 - sum({1,3,8}) = 12,
 - sum({1,3,10}) = 14,
 - sum({1,3,11}) = 15,
 - sum({1,6,8}) = 15,
 - sum({1,6,10}) = 17,
 - sum({1,6,11}) = 18,
 - sum({1,8,10}) = 19,
 - sum({1,8,11}) = 20,
 - sum({1,10,11}) = 22,
 - sum({3,6,8}) = 17,
 - sum({3,6,10}) = 19,
 - sum({3,6,11}) = 20,
 - sum({3,8,10}) = 21,
 - sum({3,8,11}) = 22,
 - sum({3,10,11}) = 24,
 - sum({6,8,10}) = 24,
 - sum({6,8,11}) = 25,
 - sum({6,10,11}) = 27,
 - sum({8,10,11}) = 29.
 -
 - Some of these sums occur more than once, others are unique.
 -
 - For a set A, let U(A,k) be the set of unique sums of k-element subsets of A,
 - in our example we find U(B,3) = {10,12,14,18,21,25,27,29} and
 - sum(U(B,3))=156.
 -
 - Now consider the 100-element set S = {1^2, 2^2, ... , 100^2}.
 - S has 100891344545564193334812497256 50-element subsets.
 -
 - Determine the sum of all integers which are the sum of exactly one of the
 - 50-element subsets of S, i.e. find sum(U(S,50)).
 -}

import Control.Monad
import Control.Monad.ST
import Data.STRef

import Data.Array.ST
import Data.Array.Unboxed

main = print solveProblem

solveProblem = let s = map (^2) [1..100]
               in sumOfUniqueSubsetSums s 50

sumOfUniqueSubsetSums xs targ = runST $ do
    arr <- generateSubsetSumCount xs targ
    ans <- countSubsets arr targ
    return ans

generateSubsetSumCount xs targ = do
    let n = length xs
        cumsums = listArray (0,n) $ scanl (+) 0 xs :: UArray Int Int
        max_sum = (cumsums ! n) - (cumsums ! (n-targ))
    arr <- newArray ((0,0), (targ,max_sum)) 0 :: ST s (STUArray s (Int,Int) Int)
    writeArray arr (0,0) 1 -- the empty subset
    forM_ (zip [1..] xs) $ \(vi,v) -> do
        let k_hi = min vi targ
        forM_ [k_hi, k_hi-1..1] $ \k -> do
            let i_hi = (cumsums ! vi) - (cumsums ! (vi-k))
            forM_ [i_hi, i_hi-1..v] $ \i -> do
                cur_ways <- readArray arr (k,i)
                prev_ways <- readArray arr (k-1,i-v)
                writeArray arr (k,i) (prev_ways + cur_ways)
    return arr


countSubsets arr targ = do
    ((_,lo), (_,hi)) <- getBounds arr
    ans <- newSTRef 0 :: ST s (STRef s Int)
    forM_ [lo..hi] $ \i -> do
        ways <- readArray arr (targ,i)
        when (ways == 1) $ do
            modifySTRef ans (+i)
    readSTRef ans
