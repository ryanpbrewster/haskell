-- juggling_with_zeros.hs
{-
 - Challenge Description:
 -
 - In this challenge you will be dealing with zero based
 - numbers. A zero based number is set in the following
 - form: "flag" "sequence of zeroes" "flag" "sequence of
 - zeroes" etc. Separated by a single space.
 -
 - 00 0 0 00 00 0
 -
 - Your goal is to convert these numbers to integers. In
 - order to do that you need to perform the following
 - steps:
 - 1. Convert a zero based number into a binary form
 - using the following rules: a) flag "0" means that the
 - following sequence of zeros should be appended to
 - a binary string. b) flag "00" means that the
 - following sequence of zeroes should be transformed
 - into a sequence of ones and appended to a binary
 - string.
 -
 - 00 0 0 00 00 0 --> 1001
 -
 - 2. Convert the obtained binary string into an
 - integer.
 -
 - 1001 --> 9
 -
 - Input sample:
 -
 - Your program should accept as its first argument
 - a path to a filename. Each line of input contains
 - a string with zero based number. E.g.
 - Output sample:
 -
 - For each line from input, print an integer
 - representation of a zero based number. E.g. 
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = map words $ lines txt
                       anss = [ binToInt $ zerosToBin inp | inp <- inps ]
                   in unlines $ map show anss

zerosToBin :: [String] -> String
zerosToBin [] = ""
zerosToBin ("00":x:xs) = replicate (length x) '1' ++ zerosToBin xs
zerosToBin  ("0":x:xs) = replicate (length x) '0' ++ zerosToBin xs

binToInt :: String -> Integer
binToInt = binToInt' . reverse
    where binToInt' "" = 0
          binToInt' ('0':xs) = 0 + 2 * binToInt' xs
          binToInt' ('1':xs) = 1 + 2 * binToInt' xs
