-- prime_palindrome.hs
{-
 - Write a program to determine the biggest prime palindrome under 1000.
 - Input sample:
 -
 - None
 - Output sample:
 -
 - Your program should print the largest palindrome on stdout. i.e.
 -
 - 929
 -}


primes = 2:[n | n <- [3,5..], all (/= 0) [ n `mod` p | p <- divcans n ] ]
    where divcans n = takeWhile (\p -> p*p <= n) primes

isPalindromic n = let s = show n
                  in s == reverse s
solveProblem bound = maximum $ filter isPalindromic $ takeWhile (<bound) primes

main = print $ solveProblem 1000
