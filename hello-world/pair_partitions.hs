-- pair_partitions.hs
-- Includes a function to split a list into two lists all possible ways


import Data.List (subsequences, (\\))

pairPartitions xs = [ (a, xs \\ a) | a <- subsequences xs ]

intListPartitions [] = [ ([],[]) ]
intListPartitions (x:xs) = let ilps = intListPartitions xs
                           in [ (i:a,(x-i):b) | (a,b) <- ilps, i <- [0..x] ]
