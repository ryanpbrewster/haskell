module Problems.P149 (solve) where

{-
 - Looking at the table below, it is easy to verify that the maximum possible
 - sum of adjacent numbers in any direction (horizontal, vertical, diagonal or
 - anti-diagonal) is 16 (= 8 + 7 + 1).
 -
 -     -2   5   3   2
 -      9  -6   5   1
 -      3   2   7   3
 -     -1   8  -4   8
 -
 - Now, let us repeat the search, but on a much larger scale:
 -
 - First, generate four million pseudo-random numbers using a specific form of
 - what is known as a "Lagged Fibonacci Generator":
 -
 - For 1 ≤ k ≤ 55, sk = [100003 - 200003k + 300007k3] (modulo 1000000) - 500000.
 - For 56 ≤ k ≤ 4000000, sk = [sk-24 + sk-55 + 1000000] (modulo 1000000) - 500000.
 -
 - Thus, s10 = -393027 and s100 = 86613.
 -
 - The terms of s are then arranged in a 2000×2000 table, using the first 2000
 - numbers to fill the first row (sequentially), the next 2000 numbers to fill
 - the second row, and so on.
 -
 - Finally, find the greatest sum of (any number of) adjacent entries in any
 - direction (horizontal, vertical, diagonal or anti-diagonal).
 -}

import Data.List (transpose)

import Util.List (chunks)

solve :: String
solve = show $ solveProblem 2000

-- To find the max subsequence, just add up adjacent numbers
-- If at any point, the current sum is worse than just starting over,
-- just start over (hence the `max x 0`)
maxSubsequence xs = maximum $ scanl (\x -> \y -> y + max x 0) 0 xs


allseqs rs = rows rs ++ cols rs ++ diags rs ++ adiags rs
rows  rs  = rs
cols  rs  = transpose rs
diags rs  = diagonals [head rs] (tail rs)
adiags rs = diags $ reverse rs

diagonals [] [] = []
diagonals top bot = let d = map head top
                        top' = takeWhile (not.null) $ map tail top
                        (b,bs) = splitAt 1 bot
                    in d:diagonals (b ++ top') bs

lfg = let m = 10^6
          start = [100003 - 200003*k + 300007*k^3 | k <- [1..55]]
          shift = subtract (m `div` 2) . (`mod` m)
          ans = map shift $ start ++ zipWith (+) ans (drop 31 ans)
      in ans

solveProblem n =
    let mat = take n $ chunks n lfg
    in maximum $ map maxSubsequence $ allseqs mat
