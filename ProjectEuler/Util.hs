module ProjectEuler.Util
( roll
) where

roll _ [] = []
roll k xs = let (front,back) = splitAt k xs
            in front:roll k back
