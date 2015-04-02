import Data.List (sort)
import Control.Monad

main = do
    n <- readLn :: IO Int
    k <- readLn :: IO Int
    xs <- replicateM n readLn :: IO [Int]
    print $ solveInstance k xs

solveInstance :: Int -> [Int] -> Int
solveInstance = minimumUnfairness


-- For a list X = [x1, ..., xn], find a subset S = {x_i1, ..., x_ik} where
--     unfairness(S) = max(S) - min(S) 
-- in minimized
-- To do this, sort X, then look at elements that are k apart
minimumUnfairness :: Int -> [Int] -> Int
minimumUnfairness k xs =
    let xs' = sort xs
        unfairnesses = zipWith (-) (drop (k-1) xs') xs'
    in minimum unfairnesses
