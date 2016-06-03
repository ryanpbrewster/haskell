module Main where

import Control.Monad (forM_)
import Data.List (mapAccumL, unfoldr)
import qualified System.Random.Mersenne.Pure64 as MT
import qualified Strategies

main = do
  forM_ [("e", Strategies.eStop), ("sqrt", Strategies.sqrtStop)] $ \(sn, s) -> do
    putStrLn $ "Using " ++ sn
    forM_ [("best", testBest), ("value", testValue)] $ \(tn, t) -> do
      putStrLn $ "Testing " ++ tn
      forM_ [10, 100, 1000] $ \len -> do
        print $ rollingMean (t len s) !! 10000

align :: [(Maybe Int, String)] -> String
align = concat . snd . mapAccumL align' 0
  where
  align' cur (target, s) = case target of
    Nothing -> (cur + length s, s)
    Just t -> (t + length s, replicate (t - cur) ' ' ++ s)

chunks k xs = let (f, r) = splitAt k xs in f : chunks k r

randoms seed = unfoldr (Just . MT.randomWord) (MT.pureMT seed)

constrain (lo, hi) x = lo + fromIntegral (x `mod` r)
  where r = fromIntegral (hi - lo + 1)

inputs = map (constrain (-100000, 100000)) (randoms 0)

testBest :: Int -> ([Int] -> Int) -> [Double]
testBest len f =
  [ if f inp == maximum inp then 1.0 else 0.0 | inp <- chunks len inputs ]

testValue :: Int -> ([Int] -> Int) -> [Double]
testValue len f =
  [ (fromIntegral $ f inp) / (fromIntegral $ maximum inp) | inp <- chunks len inputs ]

rollingMean :: [Double] -> [Double]
rollingMean = snd . mapAccumL roll (1, 0.0)
  where
  roll (len, tot) x = ((len+1, tot+x), tot / len)

