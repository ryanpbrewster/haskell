-- 119.hs
{-
 - The number 512 is interesting because it is equal to the sum of its digits
 - raised to some power: 5 + 1 + 2 = 8, and 83 = 512. Another example of
 - a number with this property is 614656 = 284.
 -
 - We shall define an to be the nth term of this sequence and insist that
 - a number must contain at least two digits to have a sum.
 -
 - You are given that a2 = 512 and a10 = 614656.
 -
 - Find a30.
 -}

import ProjectEuler.Util (mergeInf)
import ProjectEuler.Math (integerDigits)

main = print $ solveProblem 29

solveProblem k = ans !! k

ans = filter legit [10..]

legit n = let ds = sum $ integerDigits n
              k = round $ log (fromIntegral n) / log (fromIntegral ds)
          in ds^k == n
