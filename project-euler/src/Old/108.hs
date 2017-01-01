-- 108.hs
{-
 - In the following equation x, y, and z are positive integers.
 -     1/x + 1/y == 1/z
 - For z = 4 there are exactly three distinct solutions:
 -     1/5 + 1/20 = 1/4
 -     1/6 + 1/12 = 1/4
 -     1/8 + 1/8  = 1/4
 -
 - What is the least value of z for which the number of distinct solutions
 - exceeds one-thousand?
 -}

{-
 - Since we know x > z and y > z, we write x = z+a, y = z+b which yields
 -     1/(z+a) + 1/(z+b) == 1/z
 - --> (2z+a+b)/(z+a)(z+b) == 1/z
 - --> (z+a)(z+b) == z*(2z+a+b)
 - --> z^2 + z(a+b) + ab == 2z^2 + z(a+b)
 - --> ab == z^2
 - so any two numbers (a,b) that multiply together to form z^2 will yield
 - a solution in the form of
 -     1/(z+a) + 1/(z+b) == 1/z
 - Thus, we compute the divisors of z and pluck out the (a,b) pairs. For instance,
 - z = 5 yields Divisors[5^2] == {1,5,25}
 - --> (a,b) = { (1,25), (5,5) }
 - --> (x,y) = { (6,30), (10,10) }
 -}

{-
 - Minor optimization. Since we're looking for a number with a lot of
 - solutions, (that is, a lot of divisors), we can pretty much guarantee that
 - it will be divisible by at least 2 and 3 (so it will be a multiple of 6).
 -}

import ProjectEuler.Prime (sigma)

ndivs = sigma 0
numSolutions z = 1 + (ndivs (z^2)) `div` 2

solveProblem bound = head $ filter (\z -> numSolutions z > bound) [12,24..]

main = print $ solveProblem 1000
