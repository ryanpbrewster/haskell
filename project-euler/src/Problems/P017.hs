module Problems.P017
  ( solve
  ) where

{-
 - 017.hs
 - Project Euler problem 17
 - Find the number of non-whitespace characters necessary to write out the words
 - "one", "two", ..., "one thousand"
 -}
solve :: String
solve = show problem017

sprefix =
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

tprefix =
  [ "ten"
  , "eleven"
  , "twelve"
  , "thirteen"
  , "fourteen"
  , "fifteen"
  , "sixteen"
  , "seventeen"
  , "eighteen"
  , "nineteen"
  ]

dprefix =
  ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

hprefix =
  [ "onehundred"
  , "twohundred"
  , "threehundred"
  , "fourhundred"
  , "fivehundred"
  , "sixhundred"
  , "sevenhundred"
  , "eighthundred"
  , "ninehundred"
  ]

sthousand = "onethousand"

problem017 =
  let singles = [ss | ss <- sprefix]
      teens = [st | st <- tprefix]
      doubles =
        singles ++ teens ++ dprefix ++ [sd ++ ss | sd <- dprefix, ss <- singles]
      triples =
        doubles ++ hprefix ++ [sh ++ "and" ++ sd | sh <- hprefix, sd <- doubles]
      all = "onethousand" ++ concat triples
  in length all

decompose x
  | x == 0 = []
  | x < 10 = sprefix !! (x - 1)
  | x < 20 = tprefix !! (x - 10)
  | x < 100 = dprefix !! ((x `div` 10) - 2) ++ decompose (x `mod` 10)
  | x < 1000 =
    case x `mod` 100 of
      0 -> decompose (x `div` 100) ++ " hundred "
      otherwise ->
        decompose (x `div` 100) ++ " hundred and " ++ decompose (x `mod` 100)
  | x == 1000 = "one thousand"
  | otherwise = error "decompose is only defined for [1..1000]"
