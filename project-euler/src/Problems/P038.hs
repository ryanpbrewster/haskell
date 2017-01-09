module Problems.P038
  ( solve
  ) where

import Data.List (sort)
import Data.Maybe
{-
 - Take the number 192 and multiply it by each of 1, 2, and 3:
 -
 -     192 * 1 = 192
 -     192 * 2 = 384
 -     192 * 3 = 576
 - 
 - By concatenating each product we get the 1 to 9 pandigital, 192384576. We
 - will call 192384576 the concatenated product of 192 and (1,2,3)
 -
 - The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
 - and 5, giving the pandigital, 918273645, which is the concatenated product
 - of 9 and (1,2,3,4,5).
 -
 - What is the largest 1 to 9 pandigital 9-digit number that can be formed as
 - the concatenated product of an integer with (1,2, ... , n) where n > 1?
 -}
import qualified Util.Math as Math

solve :: String
solve = show solveProblem

-- Fuck, I can't make this look reasonable.
-- pandigitalConcatenation takes in an integer and returns
-- a (length 9) list of digits out of multiples of n
-- Ex. pandigitalConcatenation 9 == [9, 1,8, 2,7, 3,6, 4,5]
--
-- If concatenating multiples of n does not yield exactly 9 digits
-- then it returns Nothing
digitConcatenation n =
  let multiples = map (n *) [1 ..]
      mdigits = map Math.integerDigits multiples
      digits_len = map length mdigits
      accum_digits = takeWhile (<= 9) $ scanl1 (+) digits_len
  in if (last accum_digits) /= 9
       then Nothing
       else Just $ concat $ take (length accum_digits) mdigits

solveProblem =
  let digit_concatenations = mapMaybe digitConcatenation [1 .. 10 ^ 4]
      pandigitals = filter (\ds -> sort ds == [1 .. 9]) digit_concatenations
  in maximum $ map Math.fromIntegerDigits pandigitals
