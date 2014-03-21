import Data.Sequence

-- this is the most straightforward way I could get it to work
myLength seq = case viewl seq of
    EmptyL -> 0
    x :< xs -> 1 + myLength xs


-- This is how you do it using view patterns
myLength2 (viewl -> EmptyL) = 0
myLength2 (viewl -> x :< xs) = 1 + myLength2 xs
