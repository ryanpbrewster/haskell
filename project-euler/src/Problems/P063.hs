module Problems.P063 (solve) where

{-
 - The 5-digit number, 16807=75, is also a fifth power. Similarly, the 9-digit
 - number, 134217728=8^9, is a ninth power.
 -
 - How many n-digit positive integers exist which are also an nth power?
 -}

{-
 - So observe that 10^n is always (n+1) digits.
 -                 1^n is always 1 digit
 - Thus, for any particular exponent n > 1, we only care about bases [2..9]
 -
 - At some point, even 9^n will not be an n-digit number. Once that happens,
 - there are no more legit numbers.
 -}

solve :: String
solve = show solveProblem

legit (b,e) = (length $ show (b^e)) == e

upper_bound = length $ takeWhile legit [ (9,e) | e <- [1..] ]

solveProblem = let be_pairs = [ (b, e) | b <- [1..9], e <- [1..upper_bound] ]
               in length $ filter legit be_pairs
