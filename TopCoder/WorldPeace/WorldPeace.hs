module Main where

import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Maybe (fromJust)
import Data.List (sort)

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
solve (Problem k countries) = bruteForce k countries

bruteForce k countries
  | length countries < k = 0
  | otherwise =
    let (used, ignored) = splitAt k (reverse $ sort countries)
        countries' = filter (>0) $ map (subtract 1) used ++ ignored
    in 1 + bruteForce k countries'
