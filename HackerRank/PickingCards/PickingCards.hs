import Data.List (sort, foldl')
import Control.Monad

magic_mod = 1000000007

main = do
    t <- readLn :: IO Int
    inps <- replicateM t readInput
    let anss = map solveInstance inps
    mapM_ putStrLn anss

readInput = getLine >> fmap (map read . words) getLine

solveInstance = show . ways

ways xs =
    let cum_opts = cumScan xs
        true_opts = map (max 0) $ zipWith (-) cum_opts [0..]
    in productMod magic_mod $ take (length xs) true_opts

cumScan = fastScan

bfScan xs = [ length $ filter (<= k) xs | k <- [0..] ]

fastScan xs = fastScan' (sort xs) 0 0
    where fastScan'   []   cur count = repeat count
          fastScan' (y:ys) cur count
              | y <= cur  = fastScan' ys cur (count+1)
              | otherwise = count : fastScan' (y:ys) (cur+1) count

productMod m = foldl' (\cur x -> (cur*x) `mod` m) 1
