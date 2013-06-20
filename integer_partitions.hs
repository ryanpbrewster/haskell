import Data.List (sort)


-- find all the partitions of n into exactly k elements of xs (repeats allowed)
-- xs must be all positive integers
partitions n k xs = partitions' n k (reverse $ sort xs)

partitions' 0 0 xs = [[]]
partitions' n 0 xs = []
partitions' n k [] = []
partitions' n k x:xs | k*x < n = []
                     | otherwise = let with = partitions' (n-x) (k-1) (x:xs)
                                       without = partitions' n k xs
                                   in (map (x:) with) ++ without

main = print $ length $ partitions 70 10 [1..12]
