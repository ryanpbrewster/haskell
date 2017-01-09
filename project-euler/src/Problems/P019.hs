module Problems.P019
  ( solve
  ) where

{-
 - 019.hs
 - Project Euler problem 19
 - Find how many Sundays fell on the first of the month during
 - the twentieth century (1 Jan 1901 to 31 Dec 2000)?

 - 1 Jan 1900 was a Monday.
 - Thirty days has September,
 - April, June and November.
 - All the rest have thirty-one,
 - Saving February alone,
 - Which has twenty-eight, rain or shine.
 - And on leap years, twenty-nine.
 - A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
 -}
solve :: String
solve = show problem019

count e xs = length $ filter (\x -> x == e) xs

-- Sakamoto's algorithm
-- From Wikipedia
dayOfWeek year month date =
  let t = [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
      y =
        if month < 3
          then year - 1
          else year
      m = month - 1
      d = date
  in (y + y `div` 4 - y `div` 100 + y `div` 400 + (t !! m) + d) `mod` 7

problem019 = count 0 firstDaysOfEachMonth
  where
    firstDaysOfEachMonth =
      [dayOfWeek year month 1 | year <- [1901 .. 2000], month <- [1 .. 12]]
