-- 039.hs
{-
 - If p is the perimeter of a right angle triangle with integral length sides,
 - {a,b,c}, there are exactly three solutions for p = 120.
 -
 - {20,48,52}, {24,45,51}, {30,40,50}
 -
 - For which value of p <= 1000, is the number of solutions maximised?
 -}

import ProjectEuler.Math (primitiveTriples)
import Data.List (maximumBy, sort, group)
import Data.Ord (comparing)

main = print solveProblem

solveProblem = generalProblem 1000

generalProblem bound = 
    let prims = takeWhile (\t -> sum t < bound) primitiveTriples -- take the relevant primitive triples
        trips = [ map (k*) t | t <- prims, k <- [1..bound `div` (sum t)] ] -- generate the non-primitives
        perims = map sum trips
    in head $ maximumBy (comparing length) $ group $ sort $ perims
