-- NOT YET WORKING
-- 157.hs
{-
 - Consider the diophantine equation
 -     1/a + 1/b = p/10^n
 - with a, b, p, n positive integers and a ≤ b.
 -
 - For n=1 this equation has 20 solutions that are listed below:
 - 1/1+1/1=20/10    1/1+1/2=15/10   1/1+1/5=12/10   1/1+1/10=11/10  1/2+1/2=10/10
 - 1/2+1/5=7/10     1/2+1/10=6/10   1/3+1/6=5/10    1/3+1/15=4/10   1/4+1/4=5/10
 - 1/4+1/20=3/10    1/5+1/5=4/10    1/5+1/10=3/10   1/6+1/30=2/10   1/10+1/10=2/10
 - 1/11+1/110=1/10  1/12+1/60=1/10  1/14+1/35=1/10  1/15+1/30=1/10  1/20+1/20=1/10
 -
 - How many solutions has this equation for 1 ≤ n ≤ 9?
 -}

{-
 - In a previous problem we solved for how many solutions the equation
 -     1/x + 1/y == 1/z
 - had for a given z. The idea is that since we know x > z and y > z, we
 - write x = z+a, y = z+b which yields
 -     1/(z+a) + 1/(z+b) == 1/z
 - --> (2z+a+b)/(z+a)(z+b) == 1/z
 - --> (z+a)(z+b) == z*(2z+a+b)
 - --> z^2 + z(a+b) + ab == 2z^2 + z(a+b)
 - --> ab == z^2
 - so any two numbers (a,b) that multiply together to form z^2 will yield a solution
 - in the form of
 -     1/(z+a) + 1/(z+b) == 1/z
 - Thus, we compute the divisors of z and pluck out the (a,b) pairs. For instance,
 - z = 5 yields Divisors[5^2] == {1,5,25}
 - --> (a,b) = { (1,25), (5,5) }
 - --> (x,y) = { (6,30), (10,10) }
 -
 -
 - Now, on to this problem. For z = 10^1 we have solutions in the form of
 -     Divisors[z^2] == {1,2,4,5,10,20,25,50,100}
 - --> (a,b) = { (1,100), (2,50), (4,25), (5,20), (10,10) }
 - --> (x,y) = { (11,110), (12,60), (14,35), (15,25), (20,20) }
 -
 - Now, for any pair (x,y) we have
 -     1/x + 1/y == 1/z == 1/10^n
 - Clearly if x and y share a common divisor, say p, then
 -     x = p*x', y = p*y'
 - --> 1/(p*x') + 1/(p*y') == 1/z
 - --> 1/x' + 1/y' == p/z == p/10^n
 -
 - Thus, to find solutions to THIS problem we do the following:
 -     Let z = 10^n
 -     Find the divisors of z^2
 -     Grab the (a,b) pairs out of those divisors
 -     Turn them into (x,y) pairs
 -     For each pair (x,y)
 -         Compute g = GCD[x,y]
 -         For p in Divisors[g]
 -             p/x + p/y == 1/x' + 1/y' == p/z == p/10^n
 - Thus, the number of solutions is:
 -     sum [ numDivisors (gcd x y) | (x,y) <- divisorPairs z^2 ]
 -}

import ProjectEuler.Prime (divisors)

main = print solveProblem

solveProblem = sum $ [ numSolutions (10^n) | n <- [1..9] ]

numSolutions z = let divs = divisors (z^2)
                     abs = [ (a,b) | (a,b) <- zip divs (reverse divs), a <= b ]
                     xys = [ (z+a, z+b) | (a,b) <- abs ]
                     xy_sols = [ length $ divisors $ gcd x y | (x,y) <- xys ]
                 in sum xy_sols
