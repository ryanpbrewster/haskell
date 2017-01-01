module Problems.P020 (solve) where
{-
 - Find the sum of the digits of 100!
 -}

import Data.Char

solve :: String
solve = show $ problem020 100

problem020 n = sum $ map digitToInt $ show $ product [1..n]
