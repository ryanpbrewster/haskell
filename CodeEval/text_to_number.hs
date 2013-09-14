-- text_to_number.hs
{-
 -  You have a sting which contains a number represented as English text. Your
 -  task is to translate these numbers into their integer representation. The
 -  numbers can range from negative 999,999,999 to positive 999,999,999. The
 -  following is an exhaustive list of English words that your program must
 -  account for:
 -
 -  negative, zero, one, two, three, four, five, six, seven, eight, nine, ten,
 -  eleven, twelve, thirteen, fourteen, fifteen, sixteen, seventeen, eighteen,
 -  nineteen, twenty, thirty, forty, fifty, sixty, seventy, eighty, ninety,
 -  hundred, thousand, million
 -
 -  Input sample:
 -
 -  Your program should accept as its first argument a path to a filename.
 -  Input example is the following
 -
 -  fifteen
 -  negative six hundred thirty eight
 -  zero
 -  two million one hundred seven
 -
 -  * Negative numbers will be preceded by the word negative.
 -  * The word "hundred" is not used when "thousand" could be. E.g. 1500 is
 -    written "one thousand five hundred", not "fifteen hundred"  Output
 -
 -  Print results in the following way.
 -
 -  15
 -  -638
 -  0
 -  2000107
 -}


import System.Environment (getArgs)
import qualified Data.Map as M

primMap = M.fromList [("zero",      0)
                     ,("one",       1)
                     ,("two",       2)
                     ,("three",     3)
                     ,("four",      4)
                     ,("five",      5)
                     ,("six",       6)
                     ,("seven",     7)
                     ,("eight",     8)
                     ,("nine",      9)
                     ,("ten",      10)
                     ,("eleven",   11)
                     ,("twelve",   12)
                     ,("thirteen", 13)
                     ,("fourteen", 14)
                     ,("fifteen",  15)
                     ,("sixteen",  16)
                     ,("seventeen",17)
                     ,("eighteen", 18)
                     ,("nineteen", 19)
                     ,("twenty",   20)
                     ,("thirty",   30)
                     ,("forty",    40)
                     ,("fifty",    50)
                     ,("sixty",    60)
                     ,("seventy",  70)
                     ,("eighty",   80)
                     ,("ninety",   90)
                     ]

quantMap = M.fromList [("hundred",  10^2)
                      ,("thousand", 10^3)
                      ,("million",  10^6)
                      ]

ttn = textToNumber . words

textToNumber ("negative":ws) = -(textToNumber' ws 0)
textToNumber ws = textToNumber' ws 0

textToNumber' [] cur = cur
textToNumber' (w:ws) cur
    | w `M.member` primMap = textToNumber' ws (cur+(primMap M.! w))
    | otherwise =
        let q = quantMap M.! w
        in if q > 100 then cur*q + textToNumber' ws 0
                      else textToNumber' ws (cur*q)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let lns = lines txt
                       anss = map ttn lns
                   in unlines $ map show anss
