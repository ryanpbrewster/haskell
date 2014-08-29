-- roman_and_arabic.hs
{-
 - Roman and Arabic
 - Challenge Description:
 -
 - This question involves calculating the value of "aromatic" numbers which are a combination of Arabic digits and Roman numerals.
 - An aromatic number is of the form A1R1A2R2 ... AnRn , where each Ai is an
 - Arabic digit, and each Ri is a Roman numeral. Each pair AiRi contributes
 - a value described below, and by adding or subtracting these values together
 - we get the value of the entire aromatic number.
 - An Arabic digit A can be 0, 1, 2, 3, 4, 5, 6, 7, 8 or 9.
 - A Roman numeral R is one of the seven letters I, V, X, L, C, D, or M. Each Roman numeral has a base value: 1, 5, 10, 50, 500, 1000, respectively.
 - The value of a pair AR is A times the base value of R. Normally, you add up the values of the pairs to get the overall value. However, wherever there are consecutive symbols ARA`R` with R` having a strictly bigger base value than R, the value of pair AR must be substracted from the total, instead of being added.
 - For example, the number 3M1D2C has the value 3 × 1000 + 1 × 500 + 2 × 100 = 3700 and 3X2I4X has the value 3 × 10 - 2 × 1 + 4 × 10 = 68 . Write a program that computes the values of aromatic numbers.
 - Input sample:
 -
 - The input is a valid aromatic number consisting of between 2 and 20 symbols. Your program should accept as its first argument a path to a filename. E.g.:
 - Output sample:
 -
 - The output is the decimal value of the given aromatic number.
-}

import qualified Data.Map as M
import Data.Char (digitToInt)
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let lns = lines txt
                       anss = map arToInt lns
                   in unlines $ map show anss

roman_map = M.fromList [('I',   1)
                       ,('V',   5)
                       ,('X',  10)
                       ,('L',  50)
                       ,('C', 100)
                       ,('D', 500)
                       ,('M',1000)
                       ]
romanToInt r = roman_map M.! r

chunksOf k [] = []
chunksOf k xs = let (f,rs) = splitAt k xs in f : chunksOf k rs

baseMulPairs str = [ (romanToInt r, digitToInt m) | [m,r] <- chunksOf 2 str ]

base = fst
value (b,m) = b*m

arToInt str = arToInt' $ baseMulPairs str
    where arToInt' [] = 0
          arToInt' [f] = value f
          arToInt' (f:s:rs) | base s > base f = -value f + arToInt' (s:rs)
                            | otherwise       =  value f + arToInt' (s:rs)
