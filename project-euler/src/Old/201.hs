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
import Data.Array.ST
import Data.Array.Unboxed

main = print solveProblem

solveProblem = let s = map (^2) [1..100]
               in (uniqueSubsets s) !! 50

uniqueSubsets xs = runST $ do
    let s = sum xs
        l = length xs
    arr <- newArray ((0,0), (s,l)) 0 :: ST s (STUArray s (Int,Int) Int)
    writeArray arr (0,0) 1 -- the empty subset
    forM_ xs $ \v -> do
        forM_ [s, s-1..v] $ \i -> do
            forM_ [1..l] $ \k -> do
                prev_ways <- readArray arr (i-v,k-1)
                cur_ways <- readArray arr (i,k)
                writeArray arr (i,k) (prev_ways + cur_ways)
    ans <- newArray (0,l) 0 :: ST s (STUArray s Int Int)
    forM_ [0..l] $ \k -> do
        forM_ [0..s] $ \i -> do
            ways <- readArray arr (i,k)
            when (ways==1) $ do
                old_sum <- readArray ans k
                writeArray ans k (old_sum + i)
    getElems ans
