-- lazy_state.hs
{-
 - An exploration of "imperative-style" programming within Haskell, using
 - the State monad.
 -}

import qualified Control.Monad.ST as STS
import qualified Data.STRef as STRS

import qualified Control.Monad.ST.Lazy as STL
import qualified Data.STRef.Lazy as STRL

main = do
  let n = 10^7
  print $ take 20 (lazyRange n)
  print $ take 20 (strictRange n)

strictRange :: Int -> [Int]
strictRange n = STS.runST $ STRS.newSTRef 0 >>= range'
  where
  range' xref = do
    x <- STRS.readSTRef xref
    if x >= n then return [] else do
      STRS.writeSTRef xref (x+1)
      xs <- range' xref
      return (x : xs)

lazyRange :: Int -> [Int]
lazyRange n = STL.runST $ STRL.newSTRef 0 >>= range'
  where
  range' xref = do
    x <- STRL.readSTRef xref
    if x >= n then return [] else do
      STRL.writeSTRef xref (x+1)
      xs <- range' xref
      return (x : xs)
