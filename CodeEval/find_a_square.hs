-- find_a_square.hs
{-
 -  You have coordinates of four points on a plane. Check whether they make
 -  a square.
 -  Input sample:
 -
 -  Your program should accept as its first argument a path to a filename.
 -  Input example is the following
 -
 -      (1,6), (6,7), (2,7), (9,1)
 -      (4,1), (3,4), (0,5), (1,2)
 -      (4,6), (5,5), (5,6), (4,5)
 -
 -  All numbers in input are integers between 0 and 10
 -  Output sample:
 -
 -  Print results in the following way. 
 -
 -      false
 -      false
 -      true
 -}


import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (sort)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let rep_map = Map.fromList $ zip "()," "   "
                       lns = map (replace rep_map) $ lines txt
                       inps = [ map read $ words ln | ln <- lns ]
                       anss = map isSquare inps
                       outputs = [ if ans then "true" else "false" | ans <- anss ]
                   in unlines outputs

replace rep_map = map replaceIfInMap
    where replaceIfInMap ch = Map.findWithDefault ch ch rep_map

isSquare [x1,y1,x2,y2,x3,y3,x4,y4] =
    let (pt:pts') = [(x1,y1), (x2,y2), (x3,y3), (x4,y4)]
        [d12,d13,d14] = sort [ distSq pt pt' | pt' <- pts' ]
    in d13 == d12 && d14 == 2*d13

distSq (x1,y1) (x2,y2) = (x2-x1)^2 + (y2-y1)^2
