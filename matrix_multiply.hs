-- matrix_multiply.hs
{-
 - Simple program to explore matrix multiplication in Haskell.
 -}


import Data.List (transpose)
import ProjectEuler.Util (chunks)

dot v1 v2 = sum $ zipWith (*) v1 v2

mmult m1 m2 = let rs1 = m1 -- rows of m1
                  cs2 = transpose m2 -- columns of m2
              in [ [ dot r c | c <- cs2 ] | r <- rs1 ]

t1 = chunks 4 [1..16]
t2 = chunks 4 [16,15..1]
