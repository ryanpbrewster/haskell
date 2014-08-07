-- SIGN.hs
{-
 - A signed permutation of length n is some ordering of the positive integers {1,2,…,n} in which each integer is then provided with either a positive or negative sign (for the sake of simplicity, we omit the positive sign). For example, π=(5,−3,−2,1,4) is a signed permutation of length 5.
 -
 - Given: A positive integer n≤6.
 -
 - Return: The total number of signed permutations of length n, followed by a list of all such permutations (you may list the signed permutations in any order).
 - Sample Dataset
 -
 - 2
 -
 - Sample Output
 -
 - 8
 - -1 -2
 - -1 2
 - 1 -2
 - 1 2
 - -2 -1
 - -2 1
 - 2 -1
 - 2 1
 -}

{-
 - Just produce all permutations of all possible signed alphabets.
 -     There should be 2^n different alphabets, each with n! permutations
 -}

import System.Environment (getArgs)
import Data.List (permutations)

main = do
    args <- getArgs
    let n = read $ head args
    let ps = signedPerms n
    print $ length ps
    putStr $ unlines [ unwords $ map show p | p <- ps ]

signedPerms n = let alphabets = signedAlphabets [1..n]
                in concat [ permutations alph | alph <- alphabets ]

signedAlphabets [] = [[]]
signedAlphabets (a:as) = let alphs = signedAlphabets as in map (a:) alphs ++ map (-a:) alphs
