import Control.Monad

main = do
    n <- readLn :: IO Int
    lns <- replicateM n getLine
    let anss = map (sum . map read . words) lns
    mapM_ print anss
