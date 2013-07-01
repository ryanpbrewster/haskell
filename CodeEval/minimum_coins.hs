-- minimum_coins.hs
{-
 - You are given 3 coins of value 1, 3 and 5. You are also given a total which
 - you have to arrive at. Find the minimum number of coins to arrive at it.
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains a positive integer number which represents the
 - total you have to arrive at e.g.
 -
 - 11
 - 20
 -
 - Output sample:
 -
 - Print out the minimum number of coins required to arrive at the total e.g.
 -
 - 3
 - 4
 -}


import System.Environment (getArgs)
import Data.Maybe

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = map read $ lines txt
                       min_coins = minCoins [1,3,5]
                       anss = map (fromJust . (min_coins !!)) inputs
                   in unlines $ map show anss

inc (Just x) = Just (x+1)
inc Nothing = Nothing

myMin Nothing x = x
myMin x Nothing = x
myMin (Just x) (Just y) = Just (min x y)

-- <minCoins coins> returns a list with the number of coins required
-- to make any given non-negative integer. Thus,
--     let mcs = minCoins [2,4,6] in mcs !! 10
-- will print out the minimum number of coins required to make 10 using only
-- values of [2,4,6]

-- with no coins, we can only make 0
minCoins [] = Just 0:repeat Nothing

-- If we have a coin worth x, we can either do nothing --> (mcs !! i), or add
-- x to (i-x) --> (1 + mcs !! (i-x)). Whichever is better.
minCoins (x:xs) = let mcs = minCoins xs
                      (start,rest) = splitAt x mcs
                      mcs' = start ++ zipWith myMin rest (map inc mcs')
                  in mcs'
