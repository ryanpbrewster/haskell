-- collatz.hs

collatz n | even n    = n `div` 2
          | otherwise = 3*n + 1

collatzSequence n = takeWhile (>1) $ iterate collatz n

collatzLength = length . collatzSequence

maxCollatzSeq bound = let xs = [1..bound]
                          cls = map collatzLength xs
                      in maximum $ zip cls xs

maxCollatzPar bound = let xs = [1..bound]
                          cls = map collatzLength xs
                      in maximum $ zip cls xs

maxCollatz = maxCollatzSeq

main = print $ maxCollatz (10^6)
