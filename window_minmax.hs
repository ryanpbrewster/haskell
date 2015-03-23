import Data.Sequence hiding (splitAt, zipWith)

{-# LANGUAGE ViewPatterns #-}

minAdd e (viewl -> EmptyL) = singleton e
minAdd e xx@(viewr -> xs :> x)
    | e < x     = minAdd e xs
    | otherwise = xx |> e

maxAdd e (viewl -> EmptyL) = singleton e
maxAdd e xx@(viewr -> xs :> x)
    | e > x     = maxAdd e xs
    | otherwise = xx |> e


-- windowMax returns a list of window maxima
-- windowMax 3 [3,1,4,1,5,9,2,6] will yield
--     [ maximum [3,1,4]
--     , maximum [1,4,1]
--     , maximum [4,1,5]
--     ... ]
-- It does this in a kind of clever way using double-ended queues
windowMax winlen xs =
    let (a,b) = splitAt winlen xs
        w0 = foldl (flip maxAdd) empty a
    in windowHelper w0 b xs maxAdd

windowMin winlen xs =
    let (a,b) = splitAt winlen xs
        w0 = foldl (flip minAdd) empty a
    in windowHelper w0 b xs minAdd

differentials winlen xs =
    let maxs = windowMax winlen xs
        mins = windowMin winlen xs
    in zipWith (-) maxs mins



-- windowHelper window cur backset add
-- "backset" is the same list as cur, but backset but the length of the window

-- If cur == [], we are done. Give the final extremum and exit
windowHelper w@(viewl -> m :< w') [] _ _ = [m]
-- Else, process the current element, possibly remove the extremum, and go on
windowHelper w@(viewl -> m :< w') (x:xs) (b:bs) add
    | b == m    = m : windowHelper (add x w') xs bs
    | otherwise = m : windowHelper (add x w)  xs bs


