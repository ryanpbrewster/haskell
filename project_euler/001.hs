-- Project Euler problem 01
-- Find the sum of all numbers, x <= 999, for which n%3 == 0 or n%5 == 0

problem001 n = sum [ x | x <- [1..n], x `mod` 3 == 0 || x `mod` 5 == 0 ]


-- 3 + 6 + 9 + ... = Sum[i*3,{i,1,n/3}] = 3*(n/3)*(n/3+1)/2
problem001opt n = sigma n 3 + sigma n 5 - sigma n 15
                 where
                 sigma x k = let y = x `div` k in
                             k*y*(y+1)`div`2

main = print ans where ans = problem001 999
