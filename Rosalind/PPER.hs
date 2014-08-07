-- PPER.hs
{-
 - Given n objects, how many ways can you choose-and-permute exactly k of them?
 - Return your answer mod 10^6
 -}

{-
 - We want Binomial[n,k] * k! == n! / k! (n-k)! * k! == n! / (n-k)! == fallingFactorial n k
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    let [n,k] = map read args
    print $ (n `falling` k) `mod` 10^6

n `falling` k = product [n-k+1 .. n]
