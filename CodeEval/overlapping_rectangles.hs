-- overlapping_rectangles.hs
{-
 - Given two axis aligned rectangles A and B, determine if the two overlap.
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains 8 comma separated co-ordinates. The co-ordinates
 - are upper left x of A, upper left y of A, lower right x of A, lower right
 - y of A, upper left x of B, upper left y of B, lower right x of B, lower
 - right y of B. e.g.
 -
 - -3,3,-1,1,1,-1,3,-3
 - -3,3,-1,1,-2,4,2,2
 -
 - Output sample:
 -
 - Print out True or False if A and B intersect. e.g.
 -
 - False
 - True
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

data Rect = Rect { ll :: (Int,Int)
                 , ur :: (Int,Int)
                 }

solveProblem txt = let lns = lines txt
                       inputs = [ map read $ wordsBy (==',') ln | ln <- lns ]
                       anss = [ overlapping r1 r2 | [x1,y1,x2,y2,x3,y3,x4,y4] <- inputs
                                                  , let r1 = Rect (x1,y2) (x2,y1)
                                                  , let r2 = Rect (x3,y4) (x4,y3)
                              ]
                   in unlines $ map show anss

overlapping (Rect (x1l,y1l) (x1r,y1h)) (Rect (x2l,y2l) (x2r,y2h))
    | x1l > x2r || x2l > x1r = False
    | y1l > y2h || y2l > y1h = False
    | otherwise              = True
