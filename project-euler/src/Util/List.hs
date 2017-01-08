module Util.List
( chunks
, chunksBy
, maximumBy
, merge
, mergeInf
, ordTuples
, sublists
, tuples
) where

import Data.List (foldl')

-- chunks 3 [1..] == [ [1,2,3], [4,5,6], [7,8,9], ... ]
chunks _ [] = []
chunks k xs = let (front,back) = splitAt k xs
              in front:chunks k back

-- chunksBy generalizes chunks in that it allows a list of ks instead of a number
chunksBy _ [] = []
chunksBy (k:ks) xs = let (front,back) = splitAt k xs
                     in front:chunksBy ks back
maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy keyFn (a:as) = fst $ foldl' max' (a, keyFn a) as
  where
  max' (x, kx) y =
    let ky = keyFn y in if kx < ky then (y, ky) else (x, kx)

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y  = x:merge xs (y:ys)
                  | x == y = x:y:merge xs ys
                  | x > y  = y:merge (x:xs) ys

mergeInf :: Ord a => [[a]] -> [a]
mergeInf = foldr merge2Sorted []
    where merge2Sorted (x:xs) ys = x:merge xs ys

-- ordTuples yields all the k-tuples where all adjacent elements in the tuple
-- obey the predicate. For example, if pred == (<) then all tuples will be in
-- strictly ascending order (as each element will be (<) than its neighbor)
ordTuples pred 0 _ = [[]] -- only the empty tuple
ordTuples pred 1 xs = [[x] | x <- xs]
ordTuples pred k xs = [ x:t | t <- ordTuples pred (k-1) xs
                            , x <- xs
                            , x `pred` (head t) ]


-- similar to tuples, but maintains order
-- `sublists 2` will retrieve all distinct pairs from a list
sublists 0 _ = [[]]
sublists _ [] = []
sublists k (x:xs) = [ x:l | l <- sublists (k-1) xs ] ++ sublists k xs

-- tuples 2 [1,2,3] == [ [1,1], [1,2], [1,3], [2,1], [2,2], [2,3], [3,1], [3,2], [3,3] ]
tuples 0 _ = [[]] -- only the empty tuple
tuples k xs = [ x:t | t <- tuples (k-1) xs, x <- xs ]

