-- Problem 008
-- Find the largest product of 5 consecutive digits in Problem008.in
import Data.Char

-- group L n = a list of n-tuples made of elements in L
group [] n = []
group digits n = (take n digits) : group (tail digits) n
 
-- Read in a file, turn it into a big long string of digits
-- Then group them into chunks and find the biggest product
problem008 filename = do
    text <- readFile filename
    let digits = map digitToInt $ concat $ lines text
    return $ maximum $ map product $ group digits 5

main = do
    ans <- problem008 "008.in"
    print ans
