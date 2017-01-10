module Problems.P042
  ( process
  ) where

{-
 - The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1);
 - so the first ten triangle numbers are:
 -
 - 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
 -
 - By converting each letter in a word to a number corresponding to its
 - alphabetical position and adding these values we form a word value. For
 - example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word
 - value is a triangle number then we shall call the word a triangle word.
 -
 - Using 042.in, a 16K text file containing nearly two-thousand common English
 - words, how many are triangle words?
 -}
import Data.Array
import Data.Char

type FileContents = String

process :: FileContents -> String
process txt = show $ solveProblem (words txt)

bound = 26 * 10

charToDigit ch = 1 + ord ch - ord 'A'

isTriangleWord w = isTriangle $ sum $ map charToDigit w

triangleNumbers = takeWhile (< bound) [n * (n + 1) `div` 2 | n <- [1 ..]]

is_triangle =
  accumArray (||) False (1, bound) $ zip triangleNumbers (repeat True)

isTriangle = (is_triangle !)

solveProblem ws = length $ filter isTriangleWord ws
