-- largest_sub_matrix.hs
{-
 -  You have a matrix of positive and negative integers. Find a sub-matrix with
 -  the largest sum of it's elements. There is no limitation for sub-matrix
 -  dimensions. It only has to be rectangular.
 -
 -  * Each element in the matrix is in range [-100, 100].
 -  * Input matrix has an equal number of rows and columns.
 -  Input sample:
 -
 -  Your program should accept as its first argument a path to a filename. Read
 -  the matrix from this file. Example of the matrix is the following:
 -
 -  -1 -4 -5 -4
 -  -5 8 -1 3
 -  -2 1 3 2
 -  1 5 6 -9
 -
 -  * rows are separated by new-line character, columns
 -    are separated by space char
 -  * it's up to 20 rows/columns in the input file
 -
 -  After some calculations you may find that the sub-matrix with the largest
 -  sum for the input is
 -
 -  8 -1
 -  1 3
 -  5 6
 -
 -  Output sample:
 -
 -  Print out the sum of elements for the largest sub-matrix. For the given
 -  input it is
 -
 -  22
 -}

{-
 - This code seems correct. It matches the brute-force solution for many, many
 - inputs, and I have checked it several times. The brute force solution is
 - accepted as correct (albiet slow).
 -}

import System.Environment (getArgs)
import Data.Array

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let input = [ map read $ words ln | ln <- lines txt ]
                       ans = largestSubMatrixSum $ lolToArr input
                   in show ans

lolToArr xss= let (m, n) = (length xss, length $ head xss)
              in listArray ((1,1),(m,n)) $ concat xss

largestSubMatrixSum arr =
    let (_,(m,n)) = bounds arr
        cumsums = [ scanl (+) 0 [ arr ! (i,j) | j <- [1..n] ] | i <- [1..m] ]
        cs = listArray ((1,0),(m,n)) $ concat cumsums
    in maximum $ [ largestSubMatrixSum' cs i j | i <- [1..n], j <- [i..n] ]

largestSubMatrixSum' :: Array (Int,Int) Int -> Int -> Int -> Int
largestSubMatrixSum' cs i j =
    let (_,(m,n)) = bounds cs
        xs = [ cs ! (r,j) - cs ! (r,i-1) | r <- [1..m] ]
    in maxSublistSum xs

maxSublistSum :: [Int] -> Int
maxSublistSum xs = maximum $ scanl addmax 0 xs
    where addmax x y = y + max 0 x

bruteForceMax arr =
    let (_,(m,n)) = bounds arr
    in maximum [ subgridSum arr (i,j) (i',j') | i <- [1..m], i' <- [i..m]
                                              , j <- [1..n], j' <- [j..n] ]

subgridSum arr (i,j) (i',j') = sum [ arr ! (r,c) | r <- [i..i'], c <- [j..j'] ]
