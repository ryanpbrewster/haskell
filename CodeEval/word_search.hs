-- word_search.hs
{-
 - Given a 2D board and a word, find if the word exists in the grid. The word
 - can be constructed from letters of sequentially adjacent cell, where
 - adjacent cells are those horizontally or vertically neighboring. The same
 - letter cell may not be used more than once.
 -
 - Input sample:
 -
 - The board to be used may be hard coded as:
 -
 - [
 - [ABCE],
 - [SFCS],
 - [ADEE]
 - ]
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains a word. e.g.
 -
 - ASADB
 - ABCCED
 - Output sample:
 -
 - Print out True if the word exists in the board, False otherwise. e.g.
 -
 - False
 - True
 -}

import Data.Array
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = lines txt
                       anss = map ((toArray hardcoded) `contains`) inputs
                   in unlines $ map show anss


hardcoded = [ "ABCE"
            , "SFCS"
            , "ADEE" ]

toArray grid = listArray ((1,1), (length grid, length $ head grid)) (concat grid)

contains grid str = or [ contains' grid str [idx] | idx <- indices grid ]
    where contains' _ "" _ = True
          contains' grid (f:r) path@(cur:idxs)
              | not (inBounds (bounds grid) cur) = False -- out of bounds
              | (grid ! cur) /= f                = False -- string mismatch
              | cur `elem` idxs                  = False -- repeated cell use
              | otherwise = or [ contains' grid r (next:path) | next <- neighbors cur ]

inBounds ((xlo,ylo), (xhi,yhi)) (x,y) =
    xlo <= x && x <= xhi && ylo <= y && y <= yhi

neighbors (x,y) = [(x,y+1), (x-1,y), (x+1,y), (x,y-1)]
