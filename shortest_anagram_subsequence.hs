-- shortest_anagram_subsequence.hs
{-
Given a little string, L, and a big string, B, find the smallest substring of B, s, such
that s contains an anagram of L. Put another way, find the smallest substring of B, s, such
that for every character c in L, L.count(c) == s.count(c)
-}

import qualified Data.Map as M
import Debug.Trace
import Data.List (minimumBy)
import Data.Ord (comparing)

shortestAnagram :: (Show a, Ord a) => [a] -> [a] -> [a]
shortestAnagram little big = minimumBy (comparing length) (locallyMinimalAnagrams little big)

locallyMinimalAnagrams :: (Show a, Ord a) => [a] -> [a] -> [[a]]
locallyMinimalAnagrams little big =
  let
    initCounts = M.fromListWith (+) $ zip little (repeat 1)
  in expand [] big initCounts (M.size initCounts)
  where
  expand :: (Show a, Ord a) => [a] -> [a] -> M.Map a Int -> Int -> [[a]]
  expand _ [] _ _ = []
  expand cur (x:xs) counts numOverThreshold
    | not (x `M.member` counts) = expand (x:cur) xs counts numOverThreshold
    | otherwise =
      let
        c = counts M.! x
        cur' = x:cur
        counts' = M.insert x (c-1) counts
        numOverThreshold' = if c-1 == 0 then numOverThreshold-1 else numOverThreshold
      in if numOverThreshold' == 0
         then shrink (reverse cur') xs counts'
         else expand cur' xs counts' numOverThreshold'
  shrink :: (Show a, Ord a) => [a] -> [a] -> M.Map a Int -> [[a]]
  shrink (x:xs) big counts
    | not (x `M.member` counts) = shrink xs big counts
    | otherwise =
      let
        c = counts M.! x
        counts' = M.insert x (c+1) counts
      in if c == 0
         then (x:xs) : expand (reverse xs) big counts' 1
         else shrink xs big counts'
      
