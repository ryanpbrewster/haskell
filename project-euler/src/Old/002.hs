-- Project Euler problem 01
-- Find the sum of the even Fibonacci numbers <= 4,000,000

fibs = 1:1: zipWith (+) fibs (tail fibs)

problem002 n = sum $ takeWhile (<= n) $ filter (\x -> x `mod` 2 == 0) fibs

main = print ans where ans = problem002 $ 4*10^6
