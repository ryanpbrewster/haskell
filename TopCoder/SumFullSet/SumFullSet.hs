-- SumFullSet.hs

import qualified System.Environment as Env
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust)

main = do
  args <- Env.getArgs
  inps <- fmap concat (mapM parseInput args)
  mapM_ putStrLn [ if isClosed inp then "closed" else "not closed" | inp <- inps ]

parseInput :: String -> IO [[Int]]
parseInput fn = do
  lns <- fmap BS.lines (BS.readFile fn)
  return [ map (fst . fromJust . BS.readInt) (BS.words ln) | ln <- lns ]

isClosed :: [Int] -> Bool
isClosed xs = all (`elem` xs) (allPairSums xs)

allPairSums [] = []
allPairSums (x:xs) = map (+x) xs ++ allPairSums xs
