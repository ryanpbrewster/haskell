-- imperative_programming_2.hs
{-
 - An exploration of "imperative-style" programming within Haskell, using
 - the State monad.
 -}

import Control.Monad
import Control.Monad.ST.Lazy
import Data.Array.ST

main = print $ take 20 (stRange 1000000)

stRange :: Int -> [Int]
stRange n = runST $ do
  arr <- newArray (0, 0) 0 :: ST s (STArray s Int Int)
  range' arr
  where
  range' :: STArray s Int Int -> ST s [Int]
  range' arr = do
    x <- readArray arr 0
    if x >= n then return [] else do
      writeArray arr 0 (x+1)
      xs <- range' arr
      return (x : xs)
