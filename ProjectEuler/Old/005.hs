-- Project Euler problem 005
-- Find the LCM of {1,2,3,4,...,20}

-- Note: lcm is built in
problem005 :: Integer -> Integer
problem005 n = foldr lcm 1 [1..n]

main = print ans where ans = problem005 20
