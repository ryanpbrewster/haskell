module Problems.P072
  ( solve
  , bruteForceSolve
  , fastSolve
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
{-
 - Consider the fraction, n/d, where n and d are positive integers. If n<d and
 - HCF(n,d)=1, it is called a reduced proper fraction.
 -
 - If we list the set of reduced proper fractions for d ≤ 8 in ascending order
 - of size, we get:
 -
 - 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3,
 - 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
 -
 - It can be seen that there are 21 elements in this set.
 -
 - How many elements would be contained in the set of reduced proper fractions
 - for d ≤ 1,000,000?
 -}
import Data.Array.Unboxed

import qualified Util.Prime as Prime

solve :: String
solve = show $ fastSolve 1e6

bruteForceSolve bound = sum $ map Prime.phi [2 .. bound]

fastSolve :: Int -> Integer
fastSolve bound = sum $ map (fromIntegral . phi) [2 .. bound]
  where
    memo = totients bound
    phi n = memo ! n

totients :: Int -> UArray Int Int
totients bound =
  runSTUArray $ do
    smallestPrimeFactorArray <-
      newArray (2, bound) 0 :: ST s (STUArray s Int Int)
    forM_ [2 .. bound] $ \i -> do
      pi <- readArray smallestPrimeFactorArray i
      when (pi == 0) $ do
        writeArray smallestPrimeFactorArray i i
        forM_ [i * i,i * (i + 1) .. bound] $ \j -> do
          writeArray smallestPrimeFactorArray j i
    totientsArray <- newArray (1, bound) 1 :: ST s (STUArray s Int Int)
    forM_ [2 .. bound] $ \i -> do
      p <- readArray smallestPrimeFactorArray i
      let j = i `div` p
      phij <- readArray totientsArray j
      writeArray
        totientsArray
        i
        (phij *
         if j `mod` p /= 0
           then p - 1
           else p)
    return totientsArray
