import System.Random.Mersenne
import System.Environment
import Data.Word

main = do
    args <- getArgs
    let (n,s) = (read $ args !! 0, read $ args !! 1) :: (Int, Word32)
    rng <- newMTGen (Just s)
    rnds <- randoms rng :: IO [Double]
    print $ sum $ take n rnds
