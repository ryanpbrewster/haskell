module Problems.P209 (solve) where

{-
 - A k-input binary truth table is a map from k input bits (binary digits,
 - 0 [false] or 1 [true]) to 1 output bit. For example, the 2-input binary
 - truth tables for the logical AND and XOR functions are:
 -
 - |  x  |  y  | x AND y |
 - |  0  |  0  |    0    |
 - |  0  |  1  |    0    |
 - |  1  |  0  |    0    |
 - |  1  |  1  |    1    |
 - +-----+-----+---------+
 -
 - |  x  |  y  | x XOR y |
 - |  0  |  0  |    0    |
 - |  0  |  1  |    1    |
 - |  1  |  0  |    1    |
 - |  1  |  1  |    0    |
 - +-----+-----+---------+
 -
 - How many 6-input binary truth tables, τ, satisfy the formula
 - τ(a, b, c, d, e, f) AND τ(b, c, d, e, f, a XOR (b AND c)) = 0
 -
 - for all 6-bit inputs (a, b, c, d, e, f)?
 -}

{-
 - So the function
 -     f([a,b,c,d,e,f]) = [b,c,d,e,f,a `xor` (b .&. c)]
 - is bijective. Consider a slightly simpler bijection, g,
 - which maps the inputs
 -     g(0) -> 0
 -     g(1) -> 2
 -     g(2) -> 3
 -     g(3) -> 4
 -     g(4) -> 1
 -     g(5) -> 6
 -     g(6) -> 5
 -     g(7) -> 7
 - so it can be written as
 -     (0)(1234)(56)(7)
 -
 - Any functions where t(x) AND t(g(x)) == 0 must satisfy
 -     t(x) == 0 or t(g(x)) == 0
 - So for x == 0, we have t(0) == 0. There is no option.
 -        x == 1, we can have t(1) == 0 or t(2) == 0
 -        x == 2, t(2) == 0 or t(3) == 0
 - etc.
 -
 - Put g in its cycle form. Any cycle of length 1 MUST map to 0.
 - Any cycle of length > 1 can map the inputs in a number of ways.
 -     (56) --> {00, 01, 10}
 -     (56) --> 11 will not work.
 - In general, any map where there are no two adjacent 1s will work (consider
 - the first and last bit connected). In general there are ``bs[n]'' such mappings
 - for a cycle of length n. The derivation is left as an exercise, but it is
 -     bs[n] == fib[n] + fib[n-2]
 -
 - Thus, in general, break up the bijection into cycles
 - For each cycle, there are bs[n] options.
 - Thus, the answer will be
 -     product $ map bs $ cycleLengths bijection
 -}

import Data.List
import Data.Bits

import Util.Math (integerDigitsBy, fromIntegerDigitsBy)
import Util.List (tuples)

solve :: String
solve = show solveProblem

fibs = 0:1:zipWith (+) fibs (tail fibs)
bs = 0:zipWith (+) fibs (drop 2 fibs)

f :: Bits t => [t] -> [t]
f [a,b,c,d,e,f] = [b,c,d,e,f, a `xor` (b .&. c)]

padTo l xs = (replicate (l - length xs) 0) ++ xs

inputs :: [[Int]] -- a list of bit-strings
inputs  = map (padTo 6 . integerDigitsBy 2) [0..2^6-1]
inputs' = map f inputs

inps :: [Int] -- a list of ints which represent bit-strings
inps  = map (fromIntegerDigitsBy 2) inputs
inps' = map (fromIntegerDigitsBy 2) inputs'

findCycles f dom =
    let cycle i = takeWhile (/= i) $ tail $ iterate f i
        cycles = [ i:cycle i | i <- dom ]
    in nub $ map sort cycles

input_cycles = findCycles (inps' !!) inps

solveProblem = product $ map ((bs !!) . length) input_cycles
