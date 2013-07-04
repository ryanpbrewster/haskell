-- decode_numbers.hs
{-
 - You are given an encoded message containing only numbers. You are also
 - provided with the following mapping:
 -
 - A : 1
 - B : 2
 - C : 3
 - ...
 - Z : 26
 -
 - Given an encoded message, count the number of ways it can be decoded.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file is a testcase and contains an encoded message of numbers.
 - e.g.
 -
 - 12
 - 123
 -
 - You may assume that the test cases contain only numbers.
 -
 - Output sample:
 -
 - Print out the different number of ways it can be decoded. e.g.
 -
 - 2
 - 3
 -
 - NOTE: 12 could be decoded as AB(1 2) or L(12). Hence the number of ways to
 - decode 12 is 2.
 -}
{-
 - This is a slightly obfuscated Fibonacci thing (ways[n] = ways[n-1] + ways[n-2] (if 10*a[n]+a[n-1] <= 26)
 -}

import System.Environment (getArgs)
import Data.Char (digitToInt)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let inputs = [ map digitToInt ln | ln <- lines txt ]
                       anss = map waysToDecode inputs
                   in unlines $ map show anss

-- waysToDecode is a memoized version of bruteForce
--    waysToDecode' str prev_char ways1 ways2
--    If the string was "123" and you've processed the 1, then
--        str == "23", prev = '1', cur = 1, old = 1
--        <ways1> is how many ways you can decode "1"
--        <ways2> is how many ways you can decode ""
--    Since we could have taken "12" instead of just "1", we add cur+old to
--    get new
waysToDecode [] = 1
waysToDecode [x] = 1
waysToDecode (x:xs) = waysToDecode' xs x 1 1
    where waysToDecode' [] _ cur _ = cur
          waysToDecode' (x:xs) prev cur old =
              let new = if prev > 0 && x > 0 && (10*prev+x) <= 26 then cur+old else cur
              in waysToDecode' xs x new cur

bruteForce [] = 1
bruteForce [x] = 1
bruteForce (x:x':xs) | x > 0 && x' > 0 && (10*x+x') <= 26 = bruteForce (x':xs) + bruteForce xs
                     | otherwise                 = bruteForce (x':xs)
