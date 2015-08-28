import Data.List (sort, inits)
import StreamingMedian
import System.Random
import Control.Monad (forM_, when)

chunksOf :: [Int] -> [a] -> [[a]]
chunksOf [] xs = []
chunksOf (n:ns) xs =
  let (c, r) = splitAt n xs
  in c : chunksOf ns r

main = do
  let n = 100
  let prng = mkStdGen 42
  let lens = randomRs (5, 1000) prng :: [Int]
  let lists = take n $ chunksOf lens $ map fromIntegral $ (randomRs (0, 99) prng :: [Int])
  let tests = zip (map streamingMedian lists) (map bfStreamingMedian lists)
  let failed_tests = filter snd $ map (\(x,y) -> ((x,y), x /= y)) tests
  when (null failed_tests) $ putStrLn "All tests passed"
  forM_ failed_tests $ \((x,y), _) -> do
    putStrLn $ show x ++ " /= " ++ show y


bfStreamingMedian xs = map bfMedian $ tail $ inits xs
bfMedian xs =
  let ys = sort xs
      n = length xs
      mid = n `quot` 2
  in if odd n then ys !! mid
              else 0.5 * ( (ys !! mid) + (ys !! (mid-1)) )
