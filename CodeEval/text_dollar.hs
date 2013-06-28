-- text_dollar.hs
{-
 - You are given a positive integer number. This represents the sales made that
 - day in your department store. The payables department however, needs this
 - printed out in english. NOTE: The correct spelling of 40 is Forty. (NOT
 - Fourty)
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename.The
 - input file contains several lines. Each line is one test case. Each line
 - contains a positive integer. eg.
 -
 - 3
 - 10
 - 21
 - 466
 - 1234
 -
 - Output sample:
 -
 - For each set of input produce a single line of output which is the english
 - textual representation of that integer. The output should be unspaced and in
 - Camelcase. Always assume plural quantities. You can also assume that the
 - numbers are < 1000000000 (1 billion). In case of ambiguities eg. 2200 could
 - be TwoThousandTwoHundredDollars or TwentyTwoHundredDollars, always choose
 - the representation with the larger base i.e. TwoThousandTwoHundredDollars.
 - For the examples shown above, the answer would be:
 -
 - ThreeDollars
 - TenDollars
 - TwentyOneDollars
 - FourHundredSixtySixDollars
 - OneThousandTwoHundredThirtyFourDollars
 -}


import System.Environment (getArgs)
import Data.Array

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = map read $ words txt
                       outputs = [ (toEnglish inp) ++ "Dollars" | inp <- inputs ]
                   in unlines outputs

toEnglish n | n >= (10^6) = splitIntoParts n (10^6)
            | n >= (10^3) = splitIntoParts n (10^3)
            | n >= (10^2) = splitIntoParts n (10^2)
            | n >= (20)   = splitIntoParts n (10)
            | otherwise  = lowValue n

splitIntoParts n base = let (q,r) = n `quotRem` base
                        in highPart q base ++ toEnglish r

highPart n 1000000 = toEnglish n ++ "Million"
highPart n    1000 = toEnglish n ++ "Thousand"
highPart n     100 = toEnglish n ++ "Hundred"
highPart n      10 = doubleDigitName n

doubleDigitName n = double_digit_names ! n
double_digit_names = array (2,9) [(2, "Twenty")
                                 ,(3, "Thirty")
                                 ,(4, "Forty")
                                 ,(5, "Fifty")
                                 ,(6, "Sixty")
                                 ,(7, "Seventy")
                                 ,(8, "Eighty")
                                 ,(9, "Ninety") ]

lowValue n = number_names ! n
number_names = array (0,19) [(0, "")
                            ,(1, "One")
                            ,(2, "Two")
                            ,(3, "Three")
                            ,(4, "Four")
                            ,(5, "Five")
                            ,(6, "Six")
                            ,(7, "Seven")
                            ,(8, "Eight")
                            ,(9, "Nine")
                            ,(10, "Ten")
                            ,(11, "Eleven")
                            ,(12, "Twelve")
                            ,(13, "Thirteen")
                            ,(14, "Fourteen")
                            ,(15, "Fifteen")
                            ,(16, "Sixteen")
                            ,(17, "Seventeen")
                            ,(18, "Eighteen")
                            ,(19, "Nineteen")]

