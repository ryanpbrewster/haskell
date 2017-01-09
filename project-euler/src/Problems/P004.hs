module Problems.P004
  ( solve
  ) where

{-
 - A palindromic number reads the same both ways. The largest palindrome
 - made from the product of two 2-digit numbers is 9009 = 91*99.
 -
 - Find the largest palindrome made from the product of two 3-digit numbers.
 -}
solve :: String
solve = show $ solveProblem 3

isPalindromic x =
  let s = show x
  in s == reverse s

-- Find the largest palindrome made from the product of two `digit`-length
-- numbers
solveProblem digits =
  let lo = 10 ^ (digits - 1)
      hi = 10 ^ (digits) - 1
  in maximum
       [x | a <- [lo .. hi], b <- [a .. hi], let x = a * b, isPalindromic x]
