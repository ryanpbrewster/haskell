-- minesweeper.hs
{-
 - You will be given an M*N matrix. Each item in this matrix is either a '*' or
 - a '.'. A '*' indicates a mine whereas a '.' does not. The objective of the
 - challenge is to output a M*N matrix where each element contains a number
 - (except the positions which actually contain a mine which will remain as
 - '*') which indicates the number of mines adjacent to it. Notice that each
 - position has at most 8 adjacent positions e.g. left, top left, top, top
 - right, right, ...
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains M,N, a semicolon and the M*N matrix in row major
 - form. e.g.
 -
 - 3,5;**.........*...
 - 4,4;*........*......
 -
 - Output sample:
 -
 - Print out the new M*N matrix (in row major form) with each position(except
 - the ones with the mines) indicating how many adjacent mines are there. e.g.
 -
 - **100332001*100
 - *10022101*101110
 -}


import System.Environment (getArgs)
import Data.Array
import Data.Char (intToDigit)

chunksOf _ [] = []
chunksOf k xs = let (start,rest) = splitAt k xs in start : (chunksOf k rest)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy delims s = wordsBy' delims s
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)

solveProblem txt = let inputs = [ chunksOf cols grid | ln <- lines txt
                                                     , let [m,n,grid] = wordsBy ",;" ln
                                                     , let cols = read n ]
                       anss = map fillGridWithNumbers inputs
                   in unlines $ map concat anss

isMine '*' = 1
isMine  _  = 0

testgrid = ["...", ".*.", "..."]

fillGridWithNumbers :: [[Char]] -> [[Char]]
fillGridWithNumbers grid = let grid' = map (map isMine) grid -- [".*", "*."] --> [[0,1],[1,0]]
                               gns = sumOfNeighborsMatrix grid'
                           in [ zipWith writeInEmpty r1 r2 | (r1,r2) <- zip grid gns ]
    where writeInEmpty '*' n = '*'
          writeInEmpty ch  n = intToDigit n


sumOfNeighborsMatrix :: [[Int]] -> [[Int]]
sumOfNeighborsMatrix xs =
    let (m,n) = (length xs,length $ head xs)
        grid = listArray ((1,1), (m,n)) (concat xs)
        inBounds' = inBounds (1,1) (m,n)
        neighbors' = filter inBounds' . neighbors
        sumOfNeighbors = sum . map (grid !) . neighbors' 
    in [[ sumOfNeighbors (r,c) | c <- [1..n]] | r <- [1..m]]

inBounds (xlo,ylo) (xhi,yhi) (x,y) = xlo <= x && x <= xhi && ylo <= y && y <= yhi
neighbors (x,y) = [(x-1,y+1), (x,y+1), (x+1,y+1),
                   (x-1,y),            (x+1,y),
                   (x-1,y-1), (x,y-1), (x+1,y-1)]
