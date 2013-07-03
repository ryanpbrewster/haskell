-- counting_primes.hs
{-
 - Given two integers N and M, count the number of prime numbers between N and
 - M (both inclusive)
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains two comma separated positive integers. e.g.
 -
 - 2,10
 - 20,30
 - Output sample:
 -
 - Print out the number of primes between N and M (both inclusive)
 -}


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
                       anss = [ length $ primesBetween n m | [n,m] <- inputs ]
                   in unlines $ map show anss

primes = 2:[n | n <- [3,5..], all (/= 0) [ n `mod` p | p <- divcans n ] ]
    where divcans n = takeWhile (\p -> p*p <= n) primes

primesBetween n m = takeWhile (<=m) $ dropWhile (<n) primes
