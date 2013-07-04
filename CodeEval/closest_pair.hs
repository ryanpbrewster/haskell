-- closest_pair.hs
{-
 - You will be given the x/y co-ordinates of several locations. You will be
 - laying out 1 cable between two of these locations. In order to minimise the
 - cost, your task is to find the shortest distance between a pair of
 - locations, so that pair can be chosen for the cable installation.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename.The
 - input file contains several sets of input. Each set of input starts with an
 - integer N (0<=N<=10000), which denotes the number of points in this set. The
 - next N line contains the coordinates of N two-dimensional points. The first
 - of the two numbers denotes the X-coordinate and the latter denotes the
 - Y-coordinate. The input is terminated by a set whose N=0. This set should
 - not be processed. The value of the coordinates will be less than 40000 and
 - non-negative. eg.
 -
 - 5
 - 0 2
 - 6 67
 - 43 71
 - 39 107
 - 189 140
 - 0
 - Output sample:
 -
 - For each set of input produce a single line of output containing a floating
 - point number (with four digits after the decimal point) which denotes the
 - distance between the closest two points. If there is no such two points in
 - the input whose distance is less than 10000, print the line INFINITY. eg.
 -
 - 36.2215
 -}

{-
 - This is the brute-force O(n^2) solution. I know there is a O(n log(n))
 - solution, but this is a good first step, and it works quickly for small
 - inputs.
 -}


import System.Environment (getArgs)
import Text.Printf (printf)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)
sublists 0 _ = [[]]
sublists _ [] = []
sublists k (x:xx) = [x:sl | sl <- sublists (k-1) xx] ++ sublists k xx

bound = 10000

solveProblem txt = 
    let lns = [ map read $ words ln | ln <- lines txt ]
        inputs = parseInputs lns
        anss = map minDist inputs
        outputs = [ if ans >= bound then "INFINITY" else printf "%.4f" ans | ans <- anss ]
    in unlines outputs

parseInputs [[0]] = []
parseInputs ([n]:inps) = let (f,rest) = splitAt n inps in f:(parseInputs rest)

minDist xs = let pairs = sublists 2 xs
                 dists = [ dist (x1,y1) (x2,y2) | [[x1,y1],[x2,y2]] <- pairs ]
             in minimum dists

dist :: (Int, Int) -> (Int, Int) -> Double
dist (x1,y1) (x2,y2) = sqrt $ fromIntegral $ (x2-x1)^2 + (y2-y1)^2
