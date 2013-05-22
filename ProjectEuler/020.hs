-- 020.hs
-- Project Euler problem 20
--
-- Find the sum of the digits of 100!

import Data.Char

problem020 n = sum $ map digitToInt $ show $ product [1..n]

main = print ans where ans = problem020 100
