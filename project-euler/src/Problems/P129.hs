module Problems.P129
  ( solve
  ) where

{-
 - A number consisting entirely of ones is called a repunit. We shall define
 - R(k) to be a repunit of length k; for example, R(6) = 111111.
 -
 - Given that n is a positive integer and GCD(n, 10) = 1, it can be shown that
 - there always exists a value, k, for which R(k) is divisible by n, and let
 - A(n) be the least such value of k; for example, A(7) = 6 and A(41) = 5.
 -
 - The least value of n for which A(n) first exceeds ten is 17.
 -
 - Find the least value of n for which A(n) first exceeds one-million.
 -}
solve :: String
solve = show $ solveProblem (10 ^ 6)

a n = aH n 1 1

-- aH n r t k   {{ aH is the Helper function for a }}
-- n is the argument to A(n)
-- r is (R(k) `mod` n)
-- We build up k incrementally
--     R(k+1) = 10*R(k) + 1
-- so
--     R(k+1) `mod` n = (10*r + 1) `mod` n
aH n 0 k = k
aH n r k =
  let r' = (10 * r + 1) `mod` n
  in aH n r' (k + 1)

solveProblem bound =
  let candidates = filter (\n -> gcd n 10 == 1) [bound ..]
      an_pairs = zip candidates (map a candidates)
      ans = filter (\(n, an) -> an > bound) an_pairs
  in fst $ head ans
