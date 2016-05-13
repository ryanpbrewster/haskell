-- AlmostFibonacciKnapsack.hs

import qualified System.Environment as Env
import qualified Data.Map as M
import Control.Monad
import Debug.Trace

main = do
  args <- Env.getArgs
  inps <- fmap concat (mapM parseInput args)
  mapM_ (print . map fst . solve) inps

parseInput :: String -> IO [Integer]
parseInput fn = do
  lns <- fmap lines (readFile fn)
  return $ map read lns


solve :: Integer -> [(Int, Integer)]
solve n = 
  let 
    xs = takeWhile (<= n) almostFibs 
    sol = head $ construct n (reverse xs)
    idxMap = M.fromList $ zip xs [1..]
    idxs = map (idxMap M.!) sol
  in zip idxs sol

almostFibs = 2 : 3 : zipWith next almostFibs (tail almostFibs)
  where next a b = a + b - 1

construct :: Integer -> [Integer] -> [[Integer]]
construct 0 _ = [[]]
construct n [] = []
construct n (x:xs)
  | n < x = construct n xs
  | otherwise = map (x:) (construct (n - x) xs) ++ construct n xs
