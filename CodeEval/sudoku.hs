-- sudoku.hs
{-
 - Sudoku is a number-based logic puzzle. It typically comprises of a 9*9 grid
 - with digits so that each column, each row and each of the nine 3*3 sub-grids
 - that compose the grid contains all the digits from 1 to 9. For this
 - challenge, you will be given an N*N grid populated with numbers from
 - 1 through N and you have to determine if it is a valid sudoku solution. You
 - may assume that N will be either 4 or 9. The grid can be divided into square
 - regions of equal size, where the size of a region is equal to the square
 - root of a side of the entire grid. Thus for a 9*9 grid there would be
 - 9 regions of size 3*3 each.
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains the value of N, a semicolon and the sqaure matrix
 - of integers in row major form, comma delimited. e.g.
 -
 - 4;1,4,2,3,2,3,1,4,4,2,3,1,3,1,4,2
 - 4;2,1,3,2,3,2,1,4,1,4,2,3,2,3,4,1
 -
 - Output sample:
 -
 - Print out True/False if the grid is a valid sudoku layout. e.g.
 -
 - True
 - False
 -}



import Data.List (transpose, sort)
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let inputs = [ tail $ map read $ wordsBy (`elem` ",;") ln | ln <- lines txt ]
                       anss = map validSudoku inputs
                   in unlines $ map show anss


validSudoku :: [Int] -> Bool
validSudoku xs | length xs == 4^2 = checkSudoku (chunksOf 4 xs)
               | length xs == 9^2 = checkSudoku (chunksOf 9 xs)
               | otherwise        = False

checkSudoku grid = let n = length grid
                       targ = [1..n]
                       rows = grid
                       cols = transpose grid
                       regions = subgrids (intSqrt n) grid
                   in and [ sort es == targ | es <- rows ++ cols ++ regions ]

intSqrt n = round $ sqrt $ fromIntegral n

chunksOf _ [] = []
chunksOf k xs = let (f,r) = splitAt k xs in f:chunksOf k r

subgrids k grid = let rowchunks = chunksOf k grid
                      colchunks = [ map (chunksOf k) rows | rows <- rowchunks ]
                  in map concat $ concat $ map transpose colchunks

