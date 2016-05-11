-- imperative_programming_2.hs
{-
 - An exploration of "imperative-style" programming within Haskell, using
 - the State monad.
 -}

{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Control.Monad.ST
import Debug.Trace
import Data.Array.ST
import Data.Array.Unboxed
import qualified Data.Array as A
import qualified Data.Dequeue as Q

c_GRID0 = [ "....xxxxxxxxxxxx"
          , "x....xx.......xx"
          , "xxxx....xxxx..xx"
          , "x......xx.....xx"
          , "xxxxxxxxx..xxxxx"
          ]
c_GRID1 = let n = 500 in [[ if (i*n+j) `mod` 97 == 0 && j > 0 then 'x' else '.' | j <- [0..n-1]] | i <- [0..n-1]]
main = print $ take 1 $ explore c_GRID1

type Position = (Int, Int)
explore :: [[Char]] -> [Position]
explore charGrid = 
  let (m, n) = (length charGrid, length $ head charGrid)
      grid = A.listArray ((0, 0), (m-1, n-1)) (map (=='x') $ concat charGrid)
  in exploreST grid

neighbors :: Position -> [Position]
neighbors (i, j) = [ (i,j+1), (i-1,j), (i,j-1), (i+1,j) ]

explorePure :: A.Array Position Bool -> [Position]
explorePure grid0 = explore' (grid0 // [((0,0), True)]) (Q.pushBack Q.empty (0,0) :: Q.BankersDequeue Position)
  where
  (m, n) = let ((0, 0), (ihi, jhi)) = A.bounds grid0 in (ihi + 1, jhi + 1)
  inBounds (i, j) = 0 <= i && i < m && 0 <= j && j < n
  explore' grid (Q.popFront -> Nothing) = []
  explore' grid (Q.popFront -> Just (p, q)) =
    let ns = filter (\n -> inBounds n && not (grid A.! n)) (neighbors p)
        q' = foldl Q.pushBack q ns
        grid' = grid // zip ns (repeat True)
    in p : explore' grid' q'


exploreST :: A.Array Position Bool -> [Position]
exploreST grid0 = runST $ (thaw grid0 :: ST s (STUArray s Position Bool)) >>= \gm0 -> explore' gm0 (0,0)
  where
  (m, n) = let ((0, 0), (ihi, jhi)) = A.bounds grid0 in (ihi + 1, jhi + 1)
  inBounds (i, j) = 0 <= i && i < m && 0 <= j && j < n
  explore' gridMut p = do
    vis <- readArray gridMut p
    if vis then return [] else do
      writeArray gridMut p True
      rests <- mapM (explore' gridMut) (filter inBounds $ neighbors p)
      return (p : concat rests)
