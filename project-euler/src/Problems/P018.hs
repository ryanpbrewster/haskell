module Problems.P018 (process) where
{-
 - 018.hs
 - Project Euler problem 18
 - There is a triangle located in 018.in
 - Find the path from the top of the triangle to the bottom
 - which has the largest sum
 -}

type FileContents = String

process :: FileContents -> String
process txt = show $ problem018 txt

-- bestRow takes in two rows, the first one shorter than the second
-- It returns the second row, modified by adding the maximum of the
-- two numbers above
--
-- Example: [3 1 4]
--         [1 5 9 2]
-- yields:
--         [4 8 13 6]
-- Since:
--      The 1 inherits 3, yielding 4
--      The 5 inherits max(3,1), yielding 8
--      The 9 inherits max(1,4) yielding 13
--      The 2 inherits 4, yielding 6
bestRow :: [Integer] -> [Integer] -> [Integer]
bestRow a b = zipWith3 bestAdd b ([0]++a) (a++[0])
              where bestAdd x y z = x + max y z

-- maxPath takes in the whole triangle and returns the best path
-- It does this by propogating down the best choice you can make
-- at each row, using bestRow
maxPath :: [[Integer]] -> Integer
maxPath (fr:sr:rest) = maxPath $ bestRow fr sr : rest
maxPath tri = maximum $ head tri

problem018 :: FileContents -> Integer
problem018 txt =
    maxPath [ map read (words line) | line <- lines txt ]
