-- nqueens.hs
{-
 - A generalization of the 8-queens puzzle
 -
 - A very useful exploration of what Monads can do to clean up code.
 -}

import Control.Monad

main = print $ head (nQueens' 20)

nQueens n = nestM n (addOneQueen n) []
nQueens' n = nest n (concat . map (addOneQueen n)) [[]]

isSafePosition qs new = not $ any (`canAttack` new) qs
canAttack (r1,c1) (r2,c2) = r1 == r2 || c1 == c2 || abs (r2-r1) == abs (c2-c1)
safePositions n qs = filter (isSafePosition qs) [(length qs+1,i) | i <- [1..n]]

addOneQueen n qs = [ p:qs | p <- safePositions n qs ]

nestM 0 _ a = return a
nestM n m a = nestM (n-1) m a >>= m

nest 0 _ x = x
nest n f x = f $ nest (n-1) f x
