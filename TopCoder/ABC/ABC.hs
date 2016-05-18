-- ABC.hs
{-
You are given two s: N and K. Lun the dog is interested in strings that satisfy
the following conditions:

The string has exactly N characters, each of which is either 'A', 'B' or 'C'.
The string s has exactly K pairs (i, j) (0 <= i < j <= N-1) such that s[i] < s[j].
If there exists a string that satisfies the conditions, find and return any
such string. Otherwise, return an empty string.
-}

import Data.List (tails)
import Control.Monad
import qualified Data.Map as M
import Debug.Trace
import Data.Maybe (fromMaybe)

type Problem = (Int, Int)
type Solution = String

main = do
  mapM_ print $ [ head $ bfs (30, k) | k <- [0..300] ]

verify :: Problem -> Solution -> Bool
verify (n, k) str = backout str == (n, k)

backout :: Solution -> Problem
backout str = (length str, sum $ map backout' $ tails str)
  where
  backout' [] = 0
  backout' (x:xs) = length $ filter (>x) xs

solve :: Problem -> [Solution]
solve = bruteForce

bruteForce :: Problem -> [Solution]
bruteForce (n, k) = filter (verify (n,k)) (replicateM n "ABC")

dfs (n0, k0) = map reverse $ dfs' n0 k0 (0,0)
  where
  dfs' 0 k _ = if k == 0 then [""] else []
  dfs' n k (as, bs)
    | k < 0 = []
    | k == allAs n (as, bs) = [ replicate n 'A' ]
    | k == allBs n (as, bs) = [ replicate n 'B' ]
    | k == allCs n (as, bs) = [ replicate n 'C' ]
    | otherwise =
      map ('A':) (dfs' (n-1) k (as+1, bs)) ++
      map ('B':) (dfs' (n-1) (k - as) (as, bs+1)) ++
      map ('C':) (dfs' (n-1) (k - as - bs) (as, bs))
  allAs n (as, bs) = 0
  allBs n (as, bs) = n * as
  allCs n (as, bs) = n * (as + bs)

maxPossible :: Int -> (Int, Int) -> Int
maxPossible 0 _ = 0
maxPossible n (as, bs) = maximum [ maxPossible (n-1) (as+1, bs)
                                 , maxPossible (n-1) (as, bs+1) + as
                                 , maxPossible (n-1) (as, bs) + (as+bs)
                                 ]

type BFSTree = (Int, M.Map (Int, Int, Int, Int) Int)
bfsTrees = iterate nextTree $ M.singleton (0, 0, 0, 0) 1
  where
  addA tree = [ ((n+1, k, a+1, b), count) | ((n,k,a,b), count) <- M.toList tree ]
  addB tree = [ ((n+1, k+a, a, b+1), count) | ((n,k,a,b), count) <- M.toList tree ]
  addC tree = [ ((n+1, k+a+b, a, b), count) | ((n,k,a,b), count) <- M.toList tree ]
  nextTree tree = M.fromListWith (+) $ concat [ addA tree, addB tree, addC tree ]

bfs (n0, k0) = concat [ bfs' (n0, k0, a, b) "" | a <- [0..n0], b <- [0..n0-a] ]
  where
  tree = M.unions $ take (n0+1) bfsTrees
  bfs' (0,0,0,0) acc = [acc]
  bfs' (n,k,a,b) acc = case M.lookup (n,k,a,b) tree of
    Nothing -> []
    Just c -> concat [ bfs' (n-1,k,a-1,b) ('a':acc)
                     , bfs' (n-1,k-a,a,b-1) ('b':acc)
                     , bfs' (n-1,k-a-b,a,b) ('c':acc)
                     ]

