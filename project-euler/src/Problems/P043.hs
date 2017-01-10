module Problems.P043
  ( solve
  ) where

{-
 - The number, 1406357289, is a 0 to 9 pandigital number because it is made up
 - of each of the digits 0 to 9 in some order, but it also has a rather
 - interesting sub-string divisibility property.
 -
 - Let d0 be the 1st digit, d1 be the second digit, and so on. In this way, we
 - note the following:
 -     d1d2d3 = 406 is divisible by 2
 -     d2d3d4 = 063 is divisible by 3
 -     d3d4d5 = 635 is divisible by 5
 -     d4d5d6 = 357 is divisible by 7
 -     d5d6d7 = 572 is divisible by 11
 -     d6d7d8 = 728 is divisible by 13
 -     d7d8d9 = 289 is divisible by 17
 - Find the sum of all 0 to 9 pandigital numbers with this property.
-}
{-
 - This is basically the most straightforward implementation. The only
 - extra thing is the `quicktest`, which filters out the super-obvious bad
 - permutations.
 -}
import Data.List (permutations)
import Util.Math (fromIntegerDigits)
import Util.List (scoop)

solve :: String
solve = show solveProblem

legit digits =
  let substrings = map (fromIntegerDigits . reverse) (scoop 3 digits)
      divisibility = zipWith mod (tail substrings) [2, 3, 5, 7, 11, 13, 17]
  in all (== 0) divisibility

quicktest [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9] =
  (d3 `mod` 2 == 0) && (d5 `mod` 5 == 0)

solveProblem =
  let allperms = permutations [0 .. 9]
      someperms = filter quicktest allperms
      goodperms = filter legit someperms
  in sum $ map (fromIntegerDigits . reverse) goodperms
