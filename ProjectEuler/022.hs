-- 022.hs
-- Project Euler problem 22
-- Read in the file 022.in
-- Sort it
-- The i-th name has a score of i*value(name[i])
-- where value("qza") = 17 + 26 + 1
-- Find the sum of the scores of the names

import Data.List
import Data.Char

value str = sum [ (ord ch) - (ord 'A') + 1 | ch <- str ]


problem022 infile = do
    text <- readFile infile
    let names = sort $ lines text
    return $ sum [ i * value (names!!(i-1)) | i <- [1..length names] ]

main = do ans <- problem022 "022.in"
          print ans
