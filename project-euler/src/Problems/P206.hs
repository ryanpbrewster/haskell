module Problems.P206 (solve) where

{-
 - Find the unique positive integer whose square has the form
 -     1_2_3_4_5_6_7_8_9_0,
 - where each ``_'' is a single digit.
 -}

{-
 - Observe that the last digit of n^2 == 0, so n must end in a 0.
 - Thus, n^2 is divisible by (10^2) == 100, so the last two digits are just zeros.
 -
 - Let's add them in at the end, to save ourselves a bit of effort.
 -}

import Data.List (intersperse)

import Util.Math (integerDigits)

solve :: String
solve = show solveProblem

-- matchesTemplate template guess
-- Ex: matchesTemplate [1,6] [1,9,1,6] == True
--     matchesTemplate [10,6] [1,7,6] == True
matchesTemplate template guess =
    (reverse template) `isWildPrefixOf` (reverse guess)

isWildPrefixOf [] _ = True
isWildPrefixOf _ [] = False
isWildPrefixOf (t:ts) (g:gs) | t == 10 = isWildPrefixOf ts gs
                             | otherwise = (t==g) && isWildPrefixOf ts gs

findMatches [] = [0]
findMatches ts@(t:ts') = let ms = findMatches ts'
                             offset = 10^(length ts')
                             candidates = [offset*d + m | d <- [0..9], m <- ms]
                         in filter legit candidates
    where legit v = matchesTemplate ts (integerDigits (v^2))

solveProblem = let template = intersperse 10 [1,2,3,4,5,6,7,8,9]
                   candidates = findMatches template
                   legit n = length (integerDigits (n^2)) == length template
                   ans = head $ filter legit candidates
               in 10*ans
