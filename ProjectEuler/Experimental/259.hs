-- 259.hs
{-
 - A positive integer will be called reachable if it can result from an arithmetic expression obeying the following rules:
 - 
 -     * Uses the digits 1 through 9, in that order and exactly once each.
 -     * Any successive digits can be concatenated (for example, using the
 -       digits 2, 3 and 4 we obtain the number 234).
 -     * Only the four usual binary arithmetic operations (addition,
 -       subtraction, multiplication and division) are allowed.
 -     * Each operation can be used any number of times, or not at all.
 -     * Unary minus is not allowed.
 -     * Any number of (possibly nested) parentheses may be used to define the
 -       order of operations.
 - 
 - For example, 42 is reachable, since (1/23) * ((4*5)-6) * (78-9) = 42.
 - 
 - What is the sum of all positive reachable integers?
 -}

import Data.Set (elems, fromList)
import Data.Ratio

myNub = elems . fromList

numberCombos xs = map reverse $ numberCombos' (reverse xs)
    where numberCombos' [x] = [[x]]
          numberCombos' (x:xs) =
              let ncs = numberCombos' xs
              in [ x:y:ys | (y:ys) <- ncs ] ++ [ (10*y+x):ys | (y:ys) <- ncs ]

reachableNumbers :: [Ratio Integer] -> [Ratio Integer]
reachableNumbers [x] = [x]
reachableNumbers (x:y:xs) =
    let useit = reachableNumbers (x+y:xs) ++ reachableNumbers (x-y:xs) ++
                reachableNumbers (x*y:xs) ++ reachableNumbers (x/y:xs)
        loseit = concat [ if x' /= 0 then [x+x', x-x', x*x', x/x']
                          else            [x+x', x-x', x*x'] | x' <- reachableNumbers (y:xs) ]
    in myNub $ useit ++ loseit

isIntegral x = denominator x == 1

main = print solveProblem

solveProblem = let xss = numberCombos [1..9]
                   legit x = x > 0 && isIntegral x
                   ans = myNub $ concat [ filter legit $ reachableNumbers xs | xs <- xss ]
               in sum ans
