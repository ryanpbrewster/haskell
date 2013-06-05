-- 115.hs
{-
 - NOTE: This is a more difficult version of problem 114.
 -
 - A row measuring n units in length has red blocks with a minimum length of
 - m units placed on it, such that any two red blocks (which are allowed to be
 - different lengths) are separated by at least one black square.
 -
 - Let the fill-count function, F(m, n), represent the number of ways that a row
 - can be filled.
 -
 - For example, F(3, 29) = 673135 and F(3, 30) = 1089155.
 -
 - That is, for m = 3, it can be seen that n = 30 is the smallest value for which
 - the fill-count function first exceeds one million.
 -
 - In the same way, for m = 10, it can be verified that F(10, 56) = 880711 and
 - F(10, 57) = 1148904, so n = 57 is the least value for which the fill-count
 - function first exceeds one million.
 -
 - For m = 50, find the least value of n for which the fill-count function first
 - exceeds one million.
 -}

{-
 - See 114.hs for the derivation of the fill-count function
 - For a given m:
 -     B[0] == 1
 -     B[k] == B[k-1] + R[k-1]
 -
 -     R[0] == R[1] == ... == R[m-1] == 0
 -     R[k] == B[k-1] + R[k-m]
 -}


m = 50
b = [1] ++ zipWith (+) b r
r = (replicate m 0) ++ zipWith (+) b (drop (m-1) r)
f = zipWith (+) b r

solveProblem bound = length $ takeWhile (< bound) f

main = print $ solveProblem (10^6)
