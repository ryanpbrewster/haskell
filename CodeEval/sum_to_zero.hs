-- sum_to_zero.hs
{-
 - You are given an array of integers. Count the numbers of ways in which the sum of 4 elements in this array results in zero
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each line in this file consist of comma separated positive and negative integers. e.g.
 -
 - 2,3,1,0,-4,-1
 - 0,-1,3,-2
 - Output sample:
 -
 - Print out the count of the different number of ways that 4 elements sum to zero. e.g.
 -
 - 2
 - 1
 -}

{-
 - This is the brute-force approach. There are more clever ways to do this, but
 - in the end the problem is NP complete anyways.
 -}





import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let lns = lines txt
                       inputs = [ map read $ wordsBy (==',') ln | ln <- lns ]
                       anss = map (waysToMake 0 4) inputs
                   in unlines $ map show anss

-- how many ways can we make <targ> using exactly <k> elements of xs?
waysToMake targ k xs = length $ filter (==0) $ map sum $ sublists k xs

sublists 0 _ = [[]]
sublists _ [] = []
sublists k xs@(x:xx) = [x:sl' | sl' <- sublists (k-1) xx] ++ sublists k xx
