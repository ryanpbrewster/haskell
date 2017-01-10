module Problems.P016
  ( solve
  ) where

{- 016.hs
 - Project Euler problem 16
 - Find the sum of the base-b digits of n
 -}
solve :: String
solve = show $ problem016 (2 ^ 1000) 10

problem016 n b = sum $ integerDigits n b

integerDigits :: Integer -> Integer -> [Integer]
integerDigits n b
  | b <= 1 = error $ "Trying to expand in base " ++ show b ++ " < 2"
  | n < b = [n]
  | otherwise =
    let (q, r) = n `divMod` b
    in integerDigits q b ++ [r]
