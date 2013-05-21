-- NOT YET WORKING
-- 248.hs

-- partition a list L into all possible pairs that add up to L
-- Ex: [2,1] -> { ([2,1], [0,0]),
--                ([1,1], [1,0]),
--                ([0,1], [2,0]),
--                ([2,0], [0,1]),
--                ([1,0], [1,1]),
--                ([0,0], [2,1]) }
-- There should always be `product $ map (1+) xs` results
intListPartitions [] = [ ([],[]) ]
intListPartitions (x:xs) = let ilps = intListPartitions xs
                           in [ (i:a,(x-i):b) | (a,b) <- ilps, i <- [0..x] ]


targ   = [(2,10), (3,5), (5,2), (7,1), (11,1), (13,1)]
primes = map fst targ
exps   = map snd targ

exp_splits  = intListPartitions exps
unzipFactors [] = 1
unzipFactors (f:fs) = let (p,e) = f
                      in p^e * unzipFactors fs
prime_pairs = [ (p1,p2) | (exps1, exps2) <- exp_splits
                        , let t1 = zip primes exps1
                        , let t2 = zip primes exps2
                        , let p1 = 1 + unzipFactors t1
                        , let p2 = 1 + unzipFactors t2
                        , p1 > p2
              ]

