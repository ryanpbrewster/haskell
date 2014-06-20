-- LCSM.hs
{-
 - Problem
 -
 - A common substring of a collection of strings is a substring of every member
 - of the collection. We say that a common substring is a longest common
 - substring if there does not exist a longer common substring. For example,
 - "CG" is a common substring of "ACGTACGT" and "AACCGGTATA", but it is not as
 - long as possible; in this case, "GTA" is a longest common substring of
 - "ACGTACGT" and "AACCGTATA".
 -
 - Note that the longest common substring is not necessarily unique; for
 - a simple example, "AA" and "CC" are both longest common substrings of "AACC"
 - and "CCAA".
 -
 - Given: A collection of k (k≤100) DNA strings of length at most 1 kbp each in FASTA format.
 -
 - Return: A longest common substring of the collection. (If multiple solutions
 - exist, you may return any single solution.)
 -
 - Sample Dataset
 -     >Rosalind_1
 -     GATTACA
 -     >Rosalind_2
 -     TAGACCA
 -     >Rosalind_3
 -     ATACA
 -
 - Sample Output
 -     AC
 -}
import System.Environment (getArgs)
import qualified Data.Array as A
import qualified Data.Map as M
import Data.List (intercalate, maximumBy, transpose, isInfixOf, nub)
import Data.Ord (comparing)

import Rosalind.Structures
import Rosalind.Parsing (parseNucFASTAs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let fastas = parseNucFASTAs txt
                       ncl_strs = map (showSequence . getSequence) fastas
                       anss = longestCommonSubstrings ncl_strs
                   in head anss

longestCommonSubstrings :: Eq a => [[a]] -> [[a]]
longestCommonSubstrings (xs:xss) =
    let f l = null (commonSubstrings l xs xss)
        len = binarySearch f 0 (length xs + 1)
    in commonSubstrings len xs xss

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch f lo hi | lo == hi = lo
                     | otherwise =
    let mid = (lo + hi + 1) `quot` 2
    in if f mid then binarySearch f lo (mid-1)
                else binarySearch f mid hi

commonSubstrings :: Eq a => Int -> [a] -> [[a]] -> [[a]]
commonSubstrings len source strs =
    filter (`isCommon` strs) $ sublists len source

isCommon :: Eq a => [a] -> [[a]] -> Bool
isCommon substr strs = all (substr `isInfixOf`) strs

sublists :: Int -> [a] -> [[a]]
sublists 0 xs = [[]]
sublists k xs = let (f,r) = splitAt (k-1) xs in sublists' f r
    where sublists' sl [] = []
          sublists' sl (y:ys) = let sl' = sl ++ [y]
                                in sl' : sublists' (tail sl') ys
