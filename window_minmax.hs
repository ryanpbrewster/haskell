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

windowMin winlen xs =
    let (a,b) = splitAt winlen xs
        w0 = foldl (flip minAdd) empty a
    in windowMinHelper w0 b xs

-- windowMinHelper window cur backset
-- If cur == [], we are done. Give the final minimum and exit
windowMinHelper w@(viewl -> m :< w') [] _ = [m]
-- Else, process the current element, possibly remove the minimum, and go on
windowMinHelper w@(viewl -> m :< w') (x:xs) (b:bs)
    | b == m    = m : windowMinHelper (minAdd x w') xs bs
    | otherwise = m : windowMinHelper (minAdd x w)  xs bs



windowMax winlen xs =
    let (a,b) = splitAt winlen xs
        w0 = foldl (flip maxAdd) empty a
    in windowMaxHelper w0 b xs

-- windowMaxHelper window cur backset
-- See windowMinHelper for explanation
windowMaxHelper w@(viewl -> m :< w') [] _ = [m]
windowMaxHelper w@(viewl -> m :< w') (x:xs) (b:bs)
    | b == m    = m : windowMaxHelper (maxAdd x w') xs bs
    | otherwise = m : windowMaxHelper (maxAdd x w)  xs bs


differentials winlen xs =
    let maxs = windowMax winlen xs
        mins = windowMin winlen xs
    in zipWith (-) maxs mins
