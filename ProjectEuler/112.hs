-- 112.hs
{-
 - Working from left-to-right if no digit is exceeded by the digit to its left
 - it is called an increasing number; for example, 134468.
 -
 - Similarly if no digit is exceeded by the digit to its right it is called
 - a decreasing number; for example, 66420.
 -
 - We shall call a positive integer that is neither increasing nor decreasing
 - a "bouncy" number; for example, 155349.
 -
 - Clearly there cannot be any bouncy numbers below one-hundred, but just over
 - half of the numbers below one-thousand (525) are bouncy. In fact, the least
 - number for which the proportion of bouncy numbers first reaches 50% is 538.
 -
 - Surprisingly, bouncy numbers become more and more common and by the time we
 - reach 21780 the proportion of bouncy numbers is equal to 90%.
 -
 - Find the least number for which the proportion of bouncy numbers is exactly
 - 99%.
 -}

import Data.Ratio

main = print $ solveProblem (99%100)

solveProblem rat = let bouncyq = [ if isBouncy n then 1 else 0 | n <- [1..] ]
                       bouncy_count = scanl1 (+) bouncyq
                       bouncy_rats = zip bouncy_count [1..]
                       anss = filter (\(c,t) -> (c%t) == rat) bouncy_rats
                   in snd $ head anss

isBouncy n = let ds = digits n
                 diffs = zipWith (-) (init ds) (tail ds)
             in not $ all (>=0) diffs || all (<=0) diffs

digits 0 = []
digits n = let (q,r) = n `quotRem` 10 in r : digits q
