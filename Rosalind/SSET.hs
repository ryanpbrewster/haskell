-- SSET.hs
{-
 - Count the number of subsets of [1..n] for a given value of n.
 - Print the count, modulo 10^6
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    let n = read (head args)
    print $ powerMod 2 n (10^6)

powerMod x 0 _ = 1
powerMod x k m = let t = powerMod x (k `quot` 2) m
                     y = if odd k then x*t^2 else t^2
                 in y `mod` m
