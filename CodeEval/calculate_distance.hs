-- calculate_distance.hs
{-
 - You have coordinates of 2 points and need to find the distance between them.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Input
 - example is the following
 -
 -     (25, 4) (1, -6)
 -     (47, 43) (-25, -11)
 - All numbers in input are integers between -100 and 100.
 -
 - Output sample:
 -
 - Print results in the following way.
 -     26
 -     90
 -
 - You don't need to round the results you receive. 
 - They must be integer numbers between -100 and 100.
 -}


import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

atoi :: String -> Int
atoi = read

solveProblem txt = let inputs = [ map atoi $ wordsBy "(), " ln | ln <- lines txt ]
                       anss = [ distance (x1,y1) (x2,y2) | [x1,y1,x2,y2] <- inputs ]
                   in unlines $ map show anss

distance (x1,y1) (x2,y2) = let dsq = (x1-x2)^2 + (y1-y2)^2
                           in round $ sqrt $ fromIntegral dsq


wordsBy delims s = wordsBy' delims (dropWhile (`elem` delims) s)
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)
