module Problems.P014 (solve) where
{- 014.hs
 - Project Euler problem 14
 - Find the number, 1 <= k <= n, such that the Collatz sequence seeded by k is longest
 -}

solve :: String
solve = show $ slowSolve 1e6

slowSolve :: Int -> Int
slowSolve n = snd $ maximum $ [ (collatz x, x) | x <- [1..n] ]

collatz :: Int -> Int
collatz 1 = 0
collatz n = 1 + collatz (next n)

next :: Int -> Int
next n = if even n then n `div` 2 else 3*n+1
