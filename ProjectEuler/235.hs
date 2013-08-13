-- 235.hs
{-
 - Given is the arithmetic-geometric sequence u(k) = (900-3k)rk-1.
 - Let s(n) = Σk=1...nu(k).
 -
 - Find the value of r for which s(5000) = -600,000,000,000.
 -
 - Give your answer rounded to 12 places behind the decimal point.
 -}

import Text.Printf

u k r = (900-3*k) * (r**(k-1))
s n r = sum [ u k r | k <- [1..n] ]

nestUntil pred f x | pred x = x
                   | otherwise = nestUntil pred f (f x)

findRoot f = let lo = 1.0
                 hi = nestUntil (\x -> f x > 0) (2*) lo
             in findRoot' f lo hi

findRoot' f lo hi | hi - lo < 1e-13 = lo
                  | otherwise       =
    let mid = 0.5*(lo+hi)
    in if f mid >= 0
       then findRoot' f lo mid
       else findRoot' f mid hi

solveProblem = let targ = -600e9
                   f r = -(s 5000 r - targ)
                   ans = findRoot f :: Double
               in printf "%.12f" ans

main = putStrLn solveProblem
