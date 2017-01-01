module Problems.P114 (solve) where

{-
 - A row measuring seven units in length has red blocks with a minimum length of
 - three units placed on it, such that any two red blocks (which are allowed to be
 - different lengths) are separated by at least one black square. There are
 - exactly seventeen ways of doing this.
 -
 - -------    ---****     *****--
 - ----***    --****-     -******
 - ---***-    -****--     ******-
 - --***--    ****---     *******
 - -***---    --*****     ***-***
 - ***----    -*****-
 -
 - How many ways can a row measuring fifty units in length be filled?
 -
 - NOTE: Although the example above does not lend itself to the possibility, in
 - general it is permitted to mix block sizes. For example, on a row measuring
 - eight units in length you could use red (3), black (1), and red (4).
 -}

{-
 - Let B[n] be the number of ways to tile a length `n` by starting with a black
 - tile, and R[n] by starting with a red tile
 -
 -     For black, you always just place a black tile and move on
 -
 -     B[0] = 1
 -     B[n] = B[n-1] + R[n-1]
 -
 -
 -     For red, you can place a red tile of any length >= 3, but then it must
 -     be followed by a black tile
 -
 -     R[0] = R[1] = R[2] = 0
 -     R[n] = B[n-3] + B[n-4] + ...
 -          = Sum[ B[k], {k,0,n-3} ]
 -          = R[n-1] + B[n-3]
 -
 - With these definitions, the answer is given by
 -     f[n] = B[n] + R[n]
 -     f[50] = B[50] + R[50]
 -}

solve :: String
solve = show $ solveProblem 50

b = [1]     ++ zipWith (+) b r
r = [0,0,0] ++ zipWith (+) b (drop 2 r)
f = zipWith (+) b r

solveProblem n = f !! n
