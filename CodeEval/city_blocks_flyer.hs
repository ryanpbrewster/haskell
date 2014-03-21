-- city_blocks_flyer.hs
{-
 -  In our city we need to know how many blocks were impacted by a helicopter
 -  flying over our city. In our city, all of the blocks are rectangular. They
 -  are separated by N number of straight horizontal avenues that run East-West
 -  and M number of straight vertical streets which run North-South.
 -  A helicopter took off at the South-West corner of the city and flew
 -  directly to the farthest North-East corner. Your challenge is to determine
 -  how many city blocks it flew over?
 -
 -  You will be given two lists, the first one is for streets and the second
 -  one is for avenues. Each avenue and each street is represented by
 -  a distance D to itself from the helicopter's starting point. E.g.
 -
 -  On the first diagram the streets and the avenues are represented by the following lists:
 -
 -  (0,1,3,4,6) for streets
 -  (0,1,2,4) for avenues
 -
 -  The blocks the helicopter has flown over are colored dark grey.
 -  The inner region of each small rectangle is a city block. Rectangles'
 -  borders are not considered as a part of a block.
 -
 -  Input sample:
 -
 -  Your program should accept as its first argument a path to a filename. Each
 -  line in this file is one test case. Each test case will contain a list of
 -  distances for streets and a list of distances for avenues. Each list is in
 -  a brackets and the distances are separated by comma. The lists themselves
 -  are separated by a single whitespace. E.g.
 -
 -  (0,2,4,8,10,13,14,18,22,23,24,33,40,42,44,47,49,53,55,63,66,81,87,91) (0,147,220)
 -  (0,1,2,4) (0,1,3,4,5)
 -  (0,1,3,4,6) (0,1,2,4)
 -
 -  Output sample:
 -
 -  For each set of input print out the number of blocks the helicopter has
 -  flown over. E.g.
 -
 -  24
 -  6
 -  5
 -
 -  Constraints:
 -  N, M are in range [1, 100]
 -  D is in range [1, 1000] 
 -}

{-
 - My first inclination is to do something like a merge. Since the flyer always
 - goes along a path y = m*x, we can keep track of the most recent stree/avenue
 - it crossed.
 -
 - Every time it crosses a road, it necessarily enters a new block. Keep track
 - of how many times that happens and you're in business.
 -}

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Data.Ratio

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let lns = lines txt
                       inps = map parseLine lns
                       anss = [ blocksCrossed vs hs | (vs,hs) <- inps ]
                   in unlines $ map show anss

{- Input-parsing nonsense -}
pLine = do
    char '('
    updowns <- sepBy (many digit) (char ',')
    char ')'
    spaces
    char '('
    leftrights <- sepBy (many digit) (char ',')
    char ')'
    return (updowns, leftrights)

parseLine :: String -> ([Int], [Int])
parseLine input = case parse pLine "" input of
    Left _ -> error $ "Improperly formatted line: " ++ input
    Right v -> readLine v

readLine :: ([String], [String]) -> ([Int], [Int])
readLine (a,b) = (map read a, map read b)









{- Actual solution -}

blocksCrossed vs hs =
    let xf = last vs
        yf = last hs
        vs' = map (%1) vs
        hs' = map (%1) hs
        slope = yf % xf
    in blocksCrossed' slope vs' hs' - 1

blocksCrossed' :: Ratio Int -> [Ratio Int] -> [Ratio Int] -> Int
blocksCrossed' _ [] [] = 0
blocksCrossed' slope (x0:vs) (y0:hs)
    | x0*slope < y0 = 1 + blocksCrossed' slope vs (y0:hs)
    | x0*slope > y0 = 1 + blocksCrossed' slope (x0:vs) hs
    | otherwise     = 1 + blocksCrossed' slope vs hs
