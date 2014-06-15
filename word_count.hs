-- word_count.hs
{-
 - An exploration of various ways to ``count'' the number of elements in a list
 -}

{-
 - Findings: blow my mind...
 -
 - On 1e7 elements:
 -     digitCount1 --- 1.45s
 -     digitCount2 --- 4.39s
 -     digitCount3 --- 2.30s
 -
 - If we insist that we count 0..99 instead of 0..9 then the timings are
 -     digitCount1 --- 5.29s
 -     digitCount2 --- 4.39s  (unchanged)
 -     digitCount3 --- 2.33s (~unchanged)
 -}
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Char (digitToInt)


main = print $ digitCount3 foo

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) = (if x == y then 1 else 0) + count x ys

digitCount1 :: [Int] -> [(Int,Int)]
digitCount1 xs = [ (d, count d xs) | d <- [0..99] ]

digitCount2 :: [Int] -> [(Int,Int)]
digitCount2 xs = M.toList $ M.fromListWith (+) $ zip xs (repeat 1)

digitCount3 :: [Int] -> [(Int,Int)]
digitCount3 xs = A.assocs $ A.accumArray (+) 0 (0,99) $ zip xs (repeat 1)

foo :: [Int]
foo = take (10^7) $ cycle $ map digitToInt $ show (2^1000)
