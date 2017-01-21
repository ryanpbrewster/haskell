module Problems.P181
  ( solve
  ) where

import Control.Monad
import Control.Monad.ST

-- 181.hs
{-
 - Having three black objects B and one white object W they can be grouped in
 - 7 ways like this:
 -
 - (BBBW) (B,BBW) (B,B,BW) (B,B,B,W) (B,BB,W) (BBB,W) (BB,BW)
 -
 - In how many ways can sixty black objects B and forty white objects W be thus
 - grouped?
 -}
{-
 - Partition (60,40) into subsets of
 -     {(1,0), (2,0), ..., (0,1), (1,1), (2,1), (0,40), (1,40), ...}
 -
 - The following is executable Haskell code which solves this problem recurisvely (aka slowly)
        data Pair = Pair Int Int deriving (Show, Eq)

        plus :: Pair -> Pair -> Pair
        (Pair x1 y1) `plus` (Pair x2 y2) = Pair (x1+x2) (y1+y2)

        minus :: Pair -> Pair -> Pair
        (Pair x1 y1) `minus` (Pair x2 y2) = Pair (x1-x2) (y1-y2)

        waysToMake (Pair 0 0) _ = 1
        waysToMake _ [] = 0
        waysToMake p@(Pair b w) xs@(x:xs')
            | b < 0 || w < 0 = 0
            | otherwise = let useit = waysToMake (p `minus` x) xs
                              loseit = waysToMake p xs'
                          in useit + loseit

        waysToGroup p@(Pair b w) =
            let parts = tail [Pair b' w' | b' <- [0..b] , w' <- [0..w]]
            in waysToMake p parts

 - Calling waysToGroup (Pair 60 40) would take forever to complete, but
 -     waysToGroup (Pair 3 1) == 7
 -     waysToGroup (Pair 2 2) == 9
 -
 - We can make this MUCH faster by using dynamic programming and mutable arrays
 - This is my first try doing this in Haskell. It'll be fun.
 -}
{-
 - The imperative code looks like this, in C:
 -
 -     int waysToGroup(int Nb, int Nw) {
 -         int ways[Nb+1][Nw+1];
 -         memset(ways, 0, sizeof(ways));
 -         ways[0][0] = 1;
 -         for(int b=0; b <= Nb; b++) {
 -             for(int w=0; w <= Nw; w++) {
 -                 if( b==0 && w==0 ) continue;
 -                 for(int i=b; i <= Nb; i++) {
 -                     for(int j=w; j <= Nw; j++) {
 -                         ways[i][j] += ways[i-b][j-w];
 -                     }
 -                 }
 -             }
 -         } 
 -         return ways[Nb][Nw];
 -     }
 -}
import Data.Array.ST

solve :: String
solve = show solveProblem

solveProblem = waysToGroup 60 40

-- I do not exactly understand all the Monad magic going on here. The type
-- signature for the array is important, though. It won't compile without it.
waysToGroup nb nw =
  runST $ do
    ways <- newArray ((0, 0), (nb, nw)) 0 :: ST s (STUArray s (Int, Int) Int)
    writeArray ways (0, 0) 1
    forM_ [0 .. nb] $ \b -> do
      forM_ [0 .. nw] $ \w -> do
        when (b > 0 || w > 0) $ do
          forM_ [b .. nb] $ \i -> do
            forM_ [w .. nw] $ \j -> do
              prev <- readArray ways (i - b, j - w)
              cur <- readArray ways (i, j)
              writeArray ways (i, j) (prev + cur)
    ans <- readArray ways (nb, nw)
    return ans
