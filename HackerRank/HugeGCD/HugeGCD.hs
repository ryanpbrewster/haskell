-- HugeGCD.hs
-- See problem_statement.pdf

import Data.List( sort, foldl' )

main = do
    n <- readLn :: IO Int
    as <- fmap (map read . words) getLine :: IO [Int]
    m <- readLn :: IO Int
    bs <- fmap (map read . words) getLine :: IO [Int]
    print $ hugeGCD as bs

primes = 2 : filter isPrime [3,5..]
isComposite n = any (\p -> n `mod` p == 0) (takeWhile (\p -> p*p <= n) primes)
isPrime = not . isComposite

factor :: Int -> [Int]
factor n = factor' n primes
    where factor' n (p:ps) | n `mod` p == 0 = p : factor' (n `div` p) (p:ps)
                           | p*p > n        = if n > 1 then [n] else []
                           | otherwise      = factor' n ps

intersect as bs = intersectHelper (sort as) (sort bs)
    where intersectHelper [] _ = []
          intersectHelper _ [] = []
          intersectHelper (x:xs) (y:ys)
              | x < y  =     intersectHelper   xs   (y:ys)
              | x > y  =     intersectHelper (x:xs)   ys
              | x == y = x : intersectHelper   xs     ys

magic_mod = 1000000007
productMod m = foldl' (\cur x -> (cur*x) `mod` m) 1

hugeGCD as bs =
    let pas = concat $ map factor as
        pbs = concat $ map factor bs
    in productMod magic_mod $ intersect pas pbs
