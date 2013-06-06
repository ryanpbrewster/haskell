-- 149.hs
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

import Data.Array

-- To find the max subsequence, just add up adjacent numbers
-- If at any point, the current sum is worse than just starting over,
-- just start over (hence the `max x 0`)
maxSubsequence xs = foldl (\x -> \y -> y + max x 0) 0 xs

diagonals rs = let n = length rs
                   uls = [ [(rs !! j) !! (i - j) | j <- [0..i]] | i <- [0..n-1] ]
                   lrs = [ [(rs !! j) !! (n + i - 1 - j) | j <- [i..n-1]] | i <- [1..n-1] ]
               in uls ++ lrs

rs = [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]
ms = array ((0,0), (3,3)) [ ((r,c), rs !! r !! c) | r <- [0..3], c <- [0..3] ]

-- For all of these, m is the dimension-1
-- For a 4x4 matrix, the largest element is at (3,3) == (m,m)
allseqs mat m = rows mat m ++ cols mat m ++ diags mat m ++ adiags mat m
rows   mat m = [[ mat ! (r,   c  ) | c <- [0..m]]   | r <- [0..m]]
cols   mat m = [[ mat ! (r,   c  ) | r <- [0..m]]   | c <- [0..m]]
diags  mat m = [[ mat ! (r+i, i  ) | i <- [0..m-r]] | r <- [1..m]] ++
               [[ mat ! (i,   c+i) | i <- [0..m-c]] | c <- [0..m]]
adiags mat m = [[ mat ! (i,   c-i) | i <- [0..c]]   | c <- [0..m]] ++
               [[ mat ! (r+i, m-i) | i <- [0..m-r]] | r <- [1..m]]

lfg = let m = 10^6
          start = [100003 - 200003*k + 300007*k^3 | k <- [1..55]]
          shift = subtract (m `div` 2) . (`mod` m)
          ans = map shift $ start ++ zipWith (+) ans (drop 31 ans)
      in ans

{-
 - 1.1 for lfg
 - 2.5 for array
 - 2.9 for seqs
 - 5.7 for max subseq
 -}

solveProblem n =
    let m = n-1
        idxs = [ (r,c) | r <- [0..m], c <- [0..m] ]
        mat = array ((0,0), (m,m)) [ (idx, val) | (idx,val) <- zip idxs lfg ]
    in maximum $ map maxSubsequence $ allseqs mat m

main = print $ solveProblem 2000
