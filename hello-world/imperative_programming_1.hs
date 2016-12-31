-- imperative_programming_1.hs
{-
 - An exploration of "imperative-style" programming within Haskell, using
 - the State monad.
 -}

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed

import ProjectEuler.Prime (primes)

cumSum n = runSTArray $ do
    arr <- newArray (0,n) 0
    forM_ [1..n] $ \i -> do
        prev <- readArray arr (i-1)
        writeArray arr i (prev+i)
    return arr

primeqUpTo n = runSTArray $ do
    primeq <- newArray (2,n) True
    let sqrt_n = 1 + (floor $ sqrt $ fromIntegral n)
    forM_ [2..sqrt_n] $ \i -> do
        is_prime <- readArray primeq i
        when is_prime $ do
            forM_ [i^2,i^2+i..n] $ \j -> do
                writeArray primeq j False
    return primeq

primesUpTo n = let primeq = primeqUpTo n
               in filter (primeq !) [2..n]

myprimes = 2:[n | n <- [3,5..], all (>0) [n `rem` p | p <- takeWhile (\p -> p^2 <= n) myprimes ] ]

bound = round (1e7)
test1 = print $ sum $ primesUpTo bound
test2 = print $ sum $ takeWhile (<bound) primes
test3 = print $ sum $ takeWhile (<bound) myprimes

main = test3
