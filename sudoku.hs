-- sudoku.hs
{-
 - A simple recursive-descent sudoku solver, using monads.
 -}

import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.Maybe
import Data.List ((\\))

main = printBoard $ fromJust $ solvePuzzle test

chunks k [] = []
chunks k xs = let (f,r) = splitAt k xs in f:chunks k r

test :: Array (Int,Int) Int
test = listArray ((0,0),(8,8)) [0,0,0,0,0,0,0,0,0
                               ,0,0,0,0,0,0,0,0,0
                               ,0,0,0,0,0,0,0,0,0
                               ,0,0,0,0,0,0,0,0,0
                               ,0,0,0,0,0,0,0,0,0
                               ,0,0,0,0,0,0,0,0,0
                               ,0,0,0,0,0,0,0,0,0
                               ,0,0,0,0,0,0,0,0,0
                               ,0,0,0,0,0,0,0,0,0]

printBoard arr = putStr $ unlines $ chunks 9 $ concat $ map show $ elems arr
nextCell (i,j) | j == 8    = (i+1,0)
               | otherwise = (i,j+1)

solvePuzzle :: Array (Int,Int) Int -> Maybe (Array (Int,Int) Int)
solvePuzzle grid = runST $ do
    arr <- copyBoard grid
    found <- guessAndCheck arr (0,0)
    if (not found)
    then return Nothing
    else do
        ans <- freeze arr
        return (Just ans)


guessAndCheck arr (9,_) = return True
guessAndCheck arr (i,j) = do
    v <- readArray arr (i,j)
    if (v>0) then guessAndCheck arr (nextCell (i,j)) else do
    let ilo = 3*(i `quot` 3)
        jlo = 3*(j `quot` 3)
    v_row <- sequence [ readArray arr (i,j') | j' <- [0..8] ]
    v_col <- sequence [ readArray arr (i',j) | i' <- [0..8] ]
    v_block <- sequence [ readArray arr (i',j') | i' <- [ilo..ilo+2]
                                                , j' <- [jlo..jlo+2] ]
    let allowed = [1..9] \\ (v_row ++ v_col ++ v_block)
    tryAllowed arr (i,j) allowed

tryAllowed arr (i,j) [] = do
    writeArray arr (i,j) 0
    return False
tryAllowed arr (i,j) (v:vs) = do
    writeArray arr (i,j) v
    found <- guessAndCheck arr $ nextCell (i,j)
    if found then return True else tryAllowed arr (i,j) vs


copyBoard grid = do
    let ((rl,cl),(rh,ch)) = bounds grid
    arr <- newArray (bounds grid) 0 :: ST s (STArray s (Int,Int) Int)
    forM_ [rl..rh] $ \r -> do
        forM_ [cl..ch] $ \c -> do
            writeArray arr (r,c) (grid ! (r,c))
    return arr
