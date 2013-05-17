-- quicksort.hs

{-
 - Basic implementation of a really inefficient quicksort.
 - Still stunningly fast (10k elements in ~0.3 seconds)
 -}

import qualified System.Random as Rand

quicksort [] = []
quicksort (pivot:rest) = let lo = filter (< pivot) rest
                             hi = filter (> pivot) rest
                         in (quicksort lo) ++ [pivot] ++ (quicksort hi)


randomArray :: IO [Int]
randomArray = do
    g <- Rand.getStdGen
    return (Rand.randomRs (1, 1000000000) g)

main = do
    rands <- randomArray
    let shuffled_array = take 100000 rands
    print $ last (quicksort shuffled_array)
