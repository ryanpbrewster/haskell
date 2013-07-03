-- point_in_circle.hs
{-
 - Having coordinates of the center of a circle, it's radius and coordinates of a point you need to define whether this point is located inside of this circle.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Input example is the following
 -
 - Center: (2.12, -3.48); Radius: 17.22; Point: (16.21, -5)
 - Center: (5.05, -11); Radius: 21.2; Point: (-31, -45)
 - Center: (-9.86, 1.95); Radius: 47.28; Point: (6.03, -6.42)
 - All numbers in input are between -100 and 100
 -
 - Output sample:
 -
 - Print results in the following way.
 -
 - true
 - false
 - true
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

atof :: String -> Double
atof = read

solveProblem txt =
    let whitelist = (`elem` "-.0123456789")
        inputs = [ map atof $ wordsBy (not.whitelist) ln | ln <- lines txt ]
        anss = [ inCircle (cx,cy) r (x,y) | [cx,cy,r,x,y] <- inputs ]
        outputs = [ if a then "true" else "false" | a <- anss ]
    in unlines outputs

inCircle (cx,cy) r (x,y) = let dsq = (x-cx)^2 + (y-cy)^2
                           in dsq < r^2
