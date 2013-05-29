module ProjectEuler.Util
( roll
, tuples
) where

-- roll 3 [1..] == [ [1,2,3], [4,5,6], [7,8,9], ... ]
roll _ [] = []
roll k xs = let (front,back) = splitAt k xs
            in front:roll k back


-- tuples 2 [1,2,3] == [ [1,1], [1,2], [1,3], [2,1], [2,2], [2,3], [3,1], [3,2], [3,3] ]
tuples 0 _ = [[]] -- only the empty tuple
tuples k xs = [ x:t | t <- tuples (k-1) xs, x <- xs ]
