-- digit_statistics.hs
{-
 - Challenge Description:
 -
 - Given the numbers "a" and "n" find out how many times each digit from zero to nine is the last digit of the number in a sequence [ a, a2, a3, ... an-1, an ]
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each line of input contains two space separated integers "a" and "n" E.g:
 -
 - Output sample:
 -
 - For each line of input print out how many times the digits zero, one, two ... nine occur as the last digit of numbers in the sequence E.g:
 -
 -     0: 0, 1: 0, 2: 2, 3: 0, 4: 1, 5: 0, 6: 1, 7: 0, 8: 1, 9: 0
 -
 - In this example, the sequence consists of numbers 2, 4, 8, 16 and 32. Among
 - the last digits, the digit two occurs twice, and each of the digits four,
 - six and eight occurs once.
 -
 - Constraints:
 - 1 ≤ n ≤ 1 000 000 000 000,
 - 2 ≤ a ≤ 9 
 -}

{-
 - The sequence repeats. For instance
 -     [ 2^n `mod` 10 | n <- [1..] ] == [2,4,8,6, 2,4,8,6, 2,4,8,6, ...]
 -     [ 3^n `mod` 10 | n <- [1..] ] == [3,9,7,1, 3,9,7,1, 3,9,7,1, ...]
 -}





import System.Environment (getArgs)
import qualified Data.Array as A
import Data.List (genericTake, intercalate)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = [ map read $ words ln | ln <- lines txt ]
                       anss = map digitStatistics inps
                       outs = map show anss
                   in unlines outs

repeatingPart :: Integer -> [Int]
repeatingPart a = let rp =  a : takeWhile (/= a) [ a^k `mod` 10 | k <- [2..] ]
                  in map fromIntegral rp

digitStatistics :: [Integer] -> DigitCounter
digitStatistics [a,n] =
    let rp = repeatingPart a
        (repeats, extra) = n `quotRem` (fromIntegral $ length rp)
        leftover_digits = genericTake extra rp
    in repeats `times` (digitCount rp) `plus` leftover_digits



{- Helpful data structure -}
{- Works as such:
 -     let dc = digitCounter [3,1,4,1,5]
 -            == {0:0, 1:2, 2:0, 3:1, 4:1, 5:0, ...}
 -                     == [0,2,0,1,1,0,0,0,0,0]
 -     dc `plus` [4,5] == [0,2,0,1,2,1,0,0,0,0]
 -     5 `times`dc     == [0,10,0,5,5,0,0,0,0,0]
 -}
data DigitCounter = DigitCounter (A.Array Int Integer) deriving (Eq)
instance Show DigitCounter where show = showDigitCounter
showDigitCounter (DigitCounter dc) =
    intercalate ", " [ show d ++ ": " ++ show c | (d,c) <- A.assocs dc ]


newDigitCounter = DigitCounter $ A.array (0,9) [(d,0) | d <- [0..9]]
digitCount xs = newDigitCounter `plus` xs

times :: Integer -> DigitCounter -> DigitCounter
n `times` (DigitCounter dc) = DigitCounter $ A.accum (*) dc $ zip [0..9] (repeat n)

plus :: DigitCounter -> [Int] -> DigitCounter
(DigitCounter dc) `plus` digits = DigitCounter $ A.accum (+) dc $ zip digits (repeat 1)
