-- find_min.hs
{-
 - After sending smileys, John decided to play with arrays. Did you know that
 - hackers enjoy playing with arrays? John has a zero-based index array, m,
 - which contains n non-negative integers. However, only the first k values of
 - the array are known to him, and he wants to figure out the rest.
 -
 - John knows the following: for each index i, where k <= i < n, m is the
 - minimum non-negative integer which is *not* contained in the previous *k*
 - values of m.
 -
 - For example, if k = 3, n = 4 and the known values of m are [2, 3, 0], he can
 - figure out that m[3] = 1. John is very busy making the world more open and
 - connected, as such, he doesn't have time to figure out the rest of the
 - array. It is your task to help him. Given the first k values of m, calculate
 - the nth value of this array. (i.e. m[n - 1]).Because the values of n and
 - k can be very large, we use a pseudo-random number generator to calculate
 - the first k values of m. Given positive integers a, b, c and r, the known
 - values of m can be calculated as follows:
 -     m[0] = a
 -     m[i] = (b * m[i-1] + c) % r, 0 < i < k
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains 6 comma separated positive integers which are the
 - values of n,k,a,b,c,r in order. e.g.
 -
 - 97,39,34,37,656,97
 - 186,75,68,16,539,186
 - 137,49,48,17,461,137
 - 98,59,6,30,524,98
 - 46,18,7,11,9,46
 - Output sample:
 -
 - Print out the nth element of m for each test case e.g.
 -
 - 8
 - 38
 - 41
 - 40
 - 12
 -}
import Data.List ((\\))
import System.Environment (getArgs)


main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let inputs = [ map read $ wordsBy (==',') ln | ln <- lines txt ]
                       anss = [ findMin (makeTest a b c r k) k n | [n,k,a,b,c,r] <- inputs ]
                   in unlines $ map show anss




-- scoop 3 [1,2,3,4,5,6..] == [ [1,2,3], [2,3,4], [3,4,5], [4,5,6], ...]
scoop k xs = take k xs : scoop k (tail xs)

-- by brute-force, generate the infinite list described in the problem statement
makeM xs k = let m = xs ++ map next (scoop k m) in m
    where next = head . ([0..k] \\)

-- findMin uses the fact that m[i] is cyclic with cycle length <k-1>
findMin xs k n | n >= k = let m = take k $ drop k $ makeM xs k
                              n' = (n `mod` (k+1)) `mod` k
                          in m !! n'
               | otherwise = xs !! (n-1)


makeTest a b c r k = let test = a:map (\i -> (b*i+c) `mod` r) test
                     in take k test
