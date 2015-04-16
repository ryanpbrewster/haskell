import Data.Map.Strict (fromList, intersectionWith, toList)
import Control.Monad

main = do
    t <- readLn :: IO Int
    inps <- replicateM t readInput
    putStrLn $ solveProblem inps

chunksOf size [] = []
chunksOf size xs = let (f,r) = splitAt size xs in f : chunksOf size r

readInput = do
    xs <- fmap (map read . words) getLine
    return [ (k,v) | [k,v] <- chunksOf 2 xs ]

solveProblem = formatAnswer . listGCD

type ListOfPrimes = [(Int,Int)]
listGCD :: [ListOfPrimes] -> ListOfPrimes
listGCD pss =
    let ns = map fromList pss
        ans = foldl1 (intersectionWith min) ns
    in toList ans

formatAnswer :: ListOfPrimes -> String
formatAnswer ps =
    unwords [ show p ++ " " ++ show e | (p,e) <- ps ]
