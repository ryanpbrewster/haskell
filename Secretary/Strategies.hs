module Strategies
( eStop
, sqrtStop
) where

import Data.List (find)
import Data.Maybe (fromMaybe)

stop :: Int -> [Int] -> Int
stop k xs = 
  let
    (as, bs) = splitAt k xs
    bestFromAs = maximum as
    firstBRecord = find (>= bestFromAs) bs
  in fromMaybe (last xs) firstBRecord

eStop xs = stop (round $ fromIntegral (length xs) / (exp 1)) xs
sqrtStop xs = stop (round $ sqrt $ fromIntegral (length xs)) xs
