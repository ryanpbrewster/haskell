-- 099.hs
{-
 - Comparing two numbers written in index form like 2^11 and 3^7 is not
 - difficult, as any calculator would confirm that 2^11 = 2048 < 3^7 = 2187.
 -
 - However, confirming that 632382^518061 > 519432^525806 would be much more
 - difficult, as both numbers contain over three million digits.
 -
 - Using 099.in, a 22K text file containing one thousand lines with
 - a base/exponent pair on each line, determine which line number has the
 - greatest numerical value.
 -
 - NOTE: The first two lines in the file represent the numbers in the example
 - given above.
 -}

import ProjectEuler.Util (chunks)
import Data.Ord (comparing)
import Data.List (maximumBy, findIndex)
import Data.Maybe (fromJust) -- findIndex returns a Maybe index


main = do
    txt <- readFile "099.in"
    let be_pairs = chunks 2 $ map read $ words txt
    print $ solveProblem be_pairs

solveProblem be_pairs =
    let logvals = [ e * log b | [b,e] <- be_pairs ]
        best = fst $ maximumBy (comparing snd) $ zip be_pairs logvals
    in 1 + (fromJust $ findIndex (==best) be_pairs)


