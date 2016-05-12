module Main where

import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Maybe (fromJust)
import Data.List (sort)
import Debug.Trace

main = do
  filenames <- Env.getArgs
  forM_ filenames $ \fn -> do
    input <- parseInput fn
    print $ solve input

type Filename = String
type GroupSize = Int
type CountrySize = Int
data Problem = Problem GroupSize [CountrySize]
parseInput :: Filename -> IO Problem
parseInput fn = do
  wrds <- fmap BS.words (BS.readFile fn)
  let (gs:countries) = map (fst . fromJust . BS.readInt) wrds
  return $ Problem gs countries

type Solution = Int
solve :: Problem -> Solution
solve (Problem k countries) = bsOnAnswer k countries

bruteForce k countries
  | length countries < k = 0
  | otherwise =
    let (used, ignored) = splitAt k (reverse $ sort countries)
        countries' = filter (>0) $ map (subtract 1) used ++ ignored
    in 1 + bruteForce k countries'

kindaGreedy k countries
  | length countries < k = 0
  | otherwise =
    let (used, ignored) = splitAt k (reverse $ sort countries)
        numToUse = max 1 (minimum used - 1)
        countries' = filter (>0) $ map (subtract numToUse) used ++ ignored
    in numToUse + kindaGreedy k countries'

bsOnAnswer k countries = binarySearch (canFormNGroups k countries) (0, sum countries + 1)
canFormNGroups k countries n = sum (map (min n) countries) >= n*k

binarySearch f (lo, hi)
  | lo >= hi = lo
  | otherwise =
    let mid = lo + (hi - lo + 1) `div` 2
    in binarySearch f $ if f mid then (mid, hi) else (lo, mid-1)
