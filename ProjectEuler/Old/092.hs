-- 092.hs
{-
 - A number chain is created by continuously adding the square of the digits in
 - a number to form a new number until it has been seen before.
 -
 - For example,
 -
 - 44 → 32 → 13 → 10 → 1 → 1
 - 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
 -
 - Therefore any chain that arrives at 1 or 89 will become stuck in an endless
 - loop. What is most amazing is that EVERY starting number will eventually
 - arrive at 1 or 89.
 -
 - How many starting numbers below ten million will arrive at 89?
 -}

import ProjectEuler.Math (integerDigits)

endsWith89 1 = False
endsWith89 89 = True
endsWith89 n = endsWith89 (sum $ map (^2) $ integerDigits n)

solveProblem bound = length $ filter endsWith89 [1..bound]

main = print $ solveProblem (10^7)
