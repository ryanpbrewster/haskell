-- sum_of_primes.hs
{-
 - Write a program to determine the sum of the first 1000 prime numbers.
 -
 - Input sample:
 - None
 -
 - Output sample:
 - Your program should print the sum on stdout.i.e.
 - 
 - 3682913
 -}

primes = 2:[n | n <- [3,5..], all (/= 0) [ n `mod` p | p <- divcans n ] ]
    where divcans n = takeWhile (\p -> p*p <= n) primes

main = print $ sum $ take 1000 primes
