module Problems.P009 (solve) where

{-
 - A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
 -     a^2 + b^2 = c^2
 - 
 - For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
 -
 - There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 - Find the product abc.
 -}

solve :: String
solve = show $ solveProblem 1000

triplets = [ (a,b,c) | m <- [1..], n <- [1..m-1],
                       let a=m*m-n*n,
                       let b=2*m*n,
                       let c=m*m+n*n,
                       gcd a b == 1 ]


solveProblem :: Integer -> Integer
solveProblem target = let (a,b,c)   = head [ (a',b',c') | (a',b',c') <- triplets,
                                                          target `mod` (a'+b'+c') == 0 ]
                          perimeter = a+b+c
                          scale     = target `div` perimeter
                      in scale^3*a*b*c
