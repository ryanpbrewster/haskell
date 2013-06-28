-- commuting_engineer.hs
{-
 - Solve the travelling salesman problem.
 - Input looks like:
 -     1 | CodeEval 1355 Market St, SF (37.7768016, -122.4169151)
 -     2 | Yelp 706 Mission St, SF (37.7860105, -122.4025377)
 -     3 | Square 110 5th St, SF (37.7821494, -122.4058960)
 -     4 | Airbnb 99 Rhode Island St, SF (37.7689269, -122.4029053)
 -     5 | Dropbox 185 Berry St, SF (37.7768800, -122.3911496)
 -     6 | Zynga 699 8th St, SF (37.7706628, -122.4040139)
 -}

{-
 - The easiest strategy is to brute-force search all the possible permutations
 - of the vertices. This is very slow.
 -
 - The clean version of this program does not work when compiled with no
 - optimization. This is the slightly sloppy workaround. It is sped up
 - significantly by even minor compiler optimizations.
 -}

import System.Environment (getArgs)
import Data.List (minimumBy, permutations)
import Data.Ord (comparing)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

extractVertex :: String -> (String, (Double, Double))
extractVertex ln = let toks = words ln
                       name = head toks
                       loc' = drop (length toks - 2) toks
                       loc = let [x',y'] = loc' in (read$init$tail x', read$init y')
                   in (name, loc)

dist (_,(x1,y1)) (_,(x2,y2)) = (x2-x1)^2 + (y2-y1)^2

pathLength [a] = 0.0
pathLength vs@(a:b:_) = dist a b + pathLength (tail vs)

solveProblem txt = let vertices = map extractVertex $ lines txt
                       paths = [(head vertices) : path | path <- permutations (tail vertices)]
                       best_path = minimumBy (comparing pathLength) paths
                   in unlines $ map fst best_path
