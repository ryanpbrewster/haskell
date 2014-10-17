-- lettercase_percentage_ratio.hs
{-
 - Figure out what percent of characters in a string are upper/lower-case
 -}

import System.Environment (getArgs)
import Data.Char (isLower, isUpper)
import Text.Printf

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt =
    let inps = lines txt
        anss = map lowerUpperPercentages inps
        outs = map showAnswer anss
    in unlines outs

lowerUpperPercentages :: String -> (Double, Double)
lowerUpperPercentages str =
    let lo = fromIntegral $ length $ filter isLower str
        up = fromIntegral $ length $ filter isUpper str
        tot = fromIntegral $ length str
    in (lo / tot, up / tot)

showAnswer (lo, up) =
    printf "lowercase: %.2f uppercase: %.2f" (100*lo) (100*up)
