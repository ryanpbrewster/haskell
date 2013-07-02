-- minimum_path_sum.hs
{-
 - You are given an n*n matrix of integers. You can move only right and down.
 - Calculate the minimal path sum from the top left to the bottom right
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. The
 - first line will have the value of n(the size of the square matrix). This
 - will be followed by n rows of the matrix. (Integers in these rows will be
 - comma delimited). After the n rows, the pattern repeats. e.g.
 -
 - 2
 - 4,6
 - 2,8
 - 3
 - 1,2,3
 - 4,5,6
 - 7,8,9
 -
 - Output sample:
 -
 - Print out the minimum path sum for each matrix. e.g.
 -
 - 14
 - 21
 -}

{-
 - minPathSum is quick, at O(n^2), but "overly clever" in my opinion. The clearer way would
 - be to use arrays and memoize the brute-force recursive function.
 -
 - bfMinPathSum is very slow, but slightly clearer. I included it for clarity.
 -}


import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy delims s = wordsBy' delims s
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)

solveProblem txt = let lns = [ map read $ wordsBy "," ln | ln <- lines txt ]
                       inputs = parseInput lns
                       anss = map minPathSum inputs
                   in unlines $ map show anss
parseInput [] = []
parseInput ([n]:inps) = let (start,rest) = splitAt n inps
                        in start:(parseInput rest)

testmatrix = [[4,6],[2,8]]

minPathSum mat = last $ foldl joinRows (scanl1 (+) $ head mat) (tail mat)
    where n = length $ head mat

-- when joining two rows, you have two options in getting to each column
-- in the second row: go all the way in the first row, then drop down,
-- OR drop down earlier and finish going right in the second row
-- This code is kinda hard to understand without pencil and paper
joinRows r1 r2 = let best = (head r1 + head r2):zipWith min from_above from_left
                        where from_above = tail $ zipWith (+) r1 r2
                              from_left = zipWith (+) best (tail r2)
                 in best

bfMinPathSum [] = (10^10)
bfMinPathSum [[x]] = x
bfMinPathSum grid | length (head grid) == 0 = (10^10)
                  | otherwise               = let right = bfMinPathSum (map tail grid)
                                                  down  = bfMinPathSum (tail grid)
                                              in (head $ head grid) + min right down
