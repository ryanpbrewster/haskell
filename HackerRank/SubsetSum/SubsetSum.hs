import Control.Monad
import Data.List( sort )
import Data.IntMap hiding( map )

main = do
    n <- readLn :: IO Int
    arr <- fmap (map read . words) getLine :: IO [Int]
    t <- readLn :: IO Int
    inps <- replicateM t readLn :: IO [Int]
    let anss = solveAllInstances arr inps
    mapM_ print anss

solveAllInstances arr inps =
    let arr' = reverse (sort arr) -- descending order
        cumsums = scanl (+) 0 arr'
        m = fromAscList $ zip cumsums [0..]
    in map (formatOutput . flip lookupGE m) inps

formatOutput Nothing = -1
formatOutput (Just (_, n)) = n
