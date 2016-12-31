-- extremeSubarraySums.hs
{-
 - You are given an array, A, of integers
 -
 - You want to split A into two parts, B and C, in order to maximize the
 - difference between a subarray sum of B and C
 -
 - Basically, find two distinct subarrays of A where the difference between the
 - sums is as large as possible
 -}

maxSubarraySums xs = scanl max 0 $ maxSSs 0 xs
    where maxSSs  _    []   = []
          maxSSs cur (y:ys) =
              let cur' = cur+y
              in if cur' > 0 then (cur' : maxSSs cur' ys)
                             else (cur  : maxSSs  0   ys)

minSubarraySums xs = scanl min 0 $ minSSs 0 xs
    where minSSs  _    []   = []
          minSSs cur (y:ys) =
              let cur' = cur+y
              in if cur' < 0 then (cur' : minSSs cur' ys)
                             else (cur  : minSSs  0   ys)

solveProblem xs =
    let left_minsums = minSubarraySums xs
        right_maxsums = reverse $ maxSubarraySums $ reverse xs
        diffs = zipWith (-) right_maxsums left_minsums
    in maximum diffs
