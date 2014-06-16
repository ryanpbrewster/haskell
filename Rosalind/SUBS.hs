-- SUBS.hs
{-
 - Problem
 -
 - Given two strings s and t, t is a substring of s if t is contained as
 - a contiguous collection of symbols in s (as a result, t must be no longer
 - than s).
 -
 - The position of a symbol in a string is the total number of symbols found to
 - its left, including itself (e.g., the positions of all occurrences of 'U' in
 - "AUGCUUCAGAAAGGUCUUACG" are 2, 5, 6, 15, 17, and 18). The symbol at position
 - i of s is denoted by s[i].
 -
 - A substring of s can be represented as s[j:k], where j and k represent the
 - starting and ending positions of the substring in s; for example, if
 - s = "AUGCUUCAGAAAGGUCUUACG", then s[2:5] = "UGCU".
 -
 - The location of a substring s[j:k] is its beginning position j; note that
 - t will have multiple locations in s if it occurs more than once as
 - a substring of s (see the Sample below).
 -
 - Given: Two DNA strings s and t (each of length at most 1 kbp).
 -
 - Return: All locations of t as a substring of s.
 - Sample Dataset
 -
 - GATATATGCATATACTT
 - ATAT
 -
 - Sample Output
 -
 - 2 4 10
 -}


import System.Environment (getArgs)
import Data.List (isPrefixOf)
import Data.ByteString.Char8 (findSubstrings, pack)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

-- Just for fun
testMain = do
    let t = replicate  1000 'A' ++ ['C']
    let s = replicate 10000 'A' ++ ['C']
    print $ substringPositions t s
    print $ mySubstringPositions t s
    print $ mySubstringPositions2 t s

solveProblem txt = let [s,t] = lines txt
                       ans = mySubstringPositions t s
                   in unwords $ map show [ p+1 | p <- ans ]

substringPositions t s = findSubstrings (pack t) (pack s)


{- Doing it myself, the slow way -}
mySubstringPositions t [] = []
mySubstringPositions t s =
    (if t `isPrefixOf` s then [0] else []) ++ map (1+) (mySubstringPositions t (tail s))

{- Doing it myself, the fast way. -}
mySubstringPositions2 t s = mssps (convert t) (convert s)

mssps t s = let h  = hash t
                xs = rollingHash (length t) s
            in [ p | (p,x) <- zip [0..] xs, x == h ]

convert = map bpToInt
    where bpToInt 'A' = 0
          bpToInt 'C' = 1
          bpToInt 'G' = 2
          bpToInt 'T' = 3

hash xs = foldl (\h x -> 4*h + x) 0 xs

rollingHash l xs | length xs < l = []
                 | otherwise =
    let (f,r) = splitAt l xs
    in scanl (roll (4^(l-1))) (hash f) $ zip r xs

-- [a1,...,a(n)] -> [a2,...,a(n+1)]
-- h == a1*4^n + ... + a(n) -> a2*4^n + ... + a(n)*4^1 + a(n+1)
--                          -> (h - a1*4^n)*4 + a(n+1)
roll p h (new,old) = 4*(h - old*p) + new
