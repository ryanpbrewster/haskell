-- shortest_anagram_subsequence.hs
{-
Given a little string, L, and a big string, B, find the smallest substring of B, s, such
that s contains an anagram of L. Put another way, find the smallest substring of B, s, such
that for every character c in L, L.count(c) == s.count(c)
-}

import qualified Data.HashMap.Strict as M
import qualified Data.Array as A
import Debug.Trace
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Char (ord, chr)
import qualified Random.MWC.Pure as RNG
import Control.Monad.State

instance RNG.RangeRandom Char where
  range_random (a, b) s =
    let (n, s') = RNG.range_random (ord a, ord b) s
    in (chr n, s')

randomChars :: RNG.Seed -> [Char]
randomChars initSeed = randomChars' initSeed
  where
  randomChars' s = 
    let (ch, s') = RNG.range_random ('a', 'z') s
    in ch : randomChars' s'

chunks :: Int -> [a] -> [[a]]
chunks n xs = let (f, rest) = splitAt n xs in f : chunks n rest
  
main = do
  let inputs = map (splitAt 1000) $ chunks 10000 $ randomChars (RNG.seed [])
  print [ shortestAnagram little big | (little, big) <- take 10 inputs ]
  
shortestAnagram :: String -> String -> Int
shortestAnagram little big =
  let
    littleTally = M.fromListWith (+) $ zip little (repeat 1)
    bigArray = A.listArray (1, length big) big
  in minimum [ end - start | (start, end) <- locallyMinimalAnagrams littleTally bigArray ]


type Bookends = (Int, Int)
type StringArr = A.Array Int Char
type AnagramTally = M.HashMap Char Int
locallyMinimalAnagrams :: AnagramTally -> StringArr -> [Bookends]
locallyMinimalAnagrams little big = expand (1, 1) little (M.size little)
  where
  (1, n) = A.bounds big
  expand :: Bookends -> AnagramTally -> Int -> [Bookends]
  expand (lo, hi) counts numOverThreshold
    | hi > n = []
    | not ((big A.! hi) `M.member` counts) = expand (lo, hi+1) counts numOverThreshold
    | otherwise =
      let
        x = big A.! hi
        c = counts M.! x
        counts' = M.adjust (subtract 1) x counts
        numOverThreshold' = if c-1 == 0 then numOverThreshold-1 else numOverThreshold
      in if numOverThreshold' == 0
         then shrink (lo, hi+1) counts'
         else expand (lo, hi+1) counts' numOverThreshold'
  shrink :: Bookends -> AnagramTally -> [Bookends]
  shrink (lo, hi) counts
    | not ((big A.! lo) `M.member` counts) = shrink (lo+1, hi) counts
    | otherwise =
      let
        x = big A.! lo
        c = counts M.! x
        counts' = M.adjust (+1) x counts
      in if c == 0
         then (lo, hi) : expand (lo+1, hi) counts' 1
         else shrink (lo+1, hi) counts'
      
