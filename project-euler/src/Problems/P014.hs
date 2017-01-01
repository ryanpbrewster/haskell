module Problems.P014 (solve) where
{- 014.hs
 - Project Euler problem 14
 - Find the number, 1 <= k <= n, such that the Collatz sequence seeded by k is longest
 -}

solve :: String
solve = show $ problem014 (10^6)

problem014 :: Int -> Int
problem014 n = snd $ maximum $ [ (collatz x, x) | x <- [1..n] ]

collatz :: Int -> Int
collatz n = length $ takeWhile (>1) $ iterate next n

next :: Int -> Int
next n = if even n then n `div` 2 else 3*n+1
