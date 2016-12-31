import qualified ProjectEuler.Prime as Prime

primes = 2:[n | n <- [3,5..], all (/= 0) [ n `mod` p | p <- divcans n ] ]
    where divcans n = takeWhile (\p -> p*p <= n) primes

solveProblem n = sum $ take n primes

main = print $ solveProblem (10^5)
