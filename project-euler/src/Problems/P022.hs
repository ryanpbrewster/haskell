module Problems.P022 (process) where
{-
 - Read in the file 022.in
 - Sort it
 - The i-th name has a score of i*value(name[i])
 - where value("qza") = 17 + 26 + 1
 - Find the sum of the scores of the names
 -}

import Data.List
import Data.Char

type FileContents = String

process :: FileContents -> String
process txt = show $ solveProblem txt

solveProblem txt =
    let names = sort $ lines txt
    in sum [ i * value (names !! (i-1)) | i <- [1..length names] ]

value str = sum [ (ord ch) - (ord 'A') + 1 | ch <- str ]
