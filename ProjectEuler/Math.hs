module ProjectEuler.Math
( powerMod
, integerDigits
, integerDigitsBy
, fromIntegerDigits
, fromIntegerDigitsBy
, binomial
, coinCombos
, fullCoinCombos
, coinCombosP
, fromContinuedFraction
, primitiveTriples
, pascalTriangle
--, realDigits
) where

import ProjectEuler.Prime (factors)
import Data.List (nub)

powerMod _ 0 _ = 1
powerMod x y m | even y = (  (powerMod x (quot y 2) m)^2) `mod` m
               | odd y  = (x*(powerMod x (quot y 2) m)^2) `mod` m

{- This shit is a disaster. Jesus.

realDigits num den = let g = gcd num den
                         n = num `quot` g
                         d = den `quot` g
                         (int,frac) = n `quotRem` d
                         int_digits = integerDigits int
                         trans = transientLength d
                         raw_digits = realDigits' (10*frac) d
                         (zeros, rest) = span (==0) raw_digits
                         (t, repeating) = splitAt trans raw_digits
                         r = repetand repeating
                     in (int_digits, zeros ++ t, r)

repetand (x:xs) = x:(takeWhile (/=x) xs)

realDigits' num den = let (q,r) = quotRem num den
                      in q:(realDigits' (10*r) den)

count e xs = length $ filter (==e) xs

repetandLength d = foldl lcm 1 [ carmichael p | p <- nub $ factors d ]

-- Find the first n such that 10^n == 1 (mod p)
carmichael p = fromIntegral n
    where n = 1 + (length $ takeWhile (/= 1) $ iterate (\x -> 10*x `mod` p) 10)

transientLength d = let fs = factors d
                        a = count 2 fs
                        b = count 5 fs
                    in max a b
-}


-- Just so you know, integerDigits 0 == []
-- It's a weird edge case that I don't like.
integerDigits = integerDigitsBy 10

integerDigitsBy base = reverse . (integerDigitsBy_h base)

integerDigitsBy_h _ 0 = []
integerDigitsBy_h base n = let (q,r) = n `quotRem` base
                           in r:(integerDigitsBy_h base q)

fromIntegerDigits = fromIntegerDigitsBy 10

fromIntegerDigitsBy base = fromIntegerDigitsBy_h base . reverse

fromIntegerDigitsBy_h base [] = 0
fromIntegerDigitsBy_h base (x:xs) = x + base*(fromIntegerDigitsBy_h base xs)

-- binomial n k == n!/k!(n-k)!
--              == n*(n-1)*...*(n-k+1)/(1*2*...*k)
binomial n k = let k' = min k (n-k)
               in (product [n-k'+1..n]) `div` (product [1..k'])



{-
 - Coin combinations deals with problems like:
 -     How many ways can you make 100 using only elements from {1,5,10,25}
 -     What are the integer partitions of 10?
 -
 - coinCombos gives a list of the NUMBER OF COMBINATIONS
 - fullCoinCOmbos gives a list of the ACTUAL COMBINATIONS
 -}

coinCombos coins = coinCombos' coins (1:repeat 0) 0
coinCombos' [] combos front = drop front combos
coinCombos' (c:cs) combos front = let (start, rest) = splitAt c combos
                                      combos' = start ++ zipWith (+) rest combos'
                                      start' = drop front start
                                  in start' ++ (coinCombos' cs combos' c)

fullCoinCombos coins = fullCoinCombos' coins ([[]]:repeat []) 0
fullCoinCombos' [] combos front = drop front combos
fullCoinCombos' (c:cs) combos front =
    let (start, rest) = splitAt c combos
        combos' = start ++ zipWith (newCombos c) rest combos'
        start' = drop front start
    in start' ++ (fullCoinCombos' cs combos' c)
newCombos c combos1 combos2 = combos1 ++ [ c:c2 | c2 <- combos2 ]

-- proper coin combos. Can only use each coin once
coinCombosP coins = coinCombosP' coins (1:repeat 0) 0
coinCombosP' [] combos front = drop front combos
coinCombosP' (x:xs) combos front = let (start, rest) = splitAt x combos
                                       combos' = start ++ zipWith (+) rest combos
                                       start' = drop front start
                                   in start' ++ (coinCombosP' xs combos' x)

-- Returns a (n,d) pair which represents n/d in reduced form.
fromContinuedFraction [a] = (a, 1)
fromContinuedFraction (a:cfrac) = let (rn, rd) = fromContinuedFraction cfrac
                                      n = rn*a + rd
                                      d = rn
                                      g = gcd n d
                                  in (n `div` g, d `div` g)

{-
 - From Wikipedia:
 - For any primitive Pythagorean triple [a,b,c], represented as a column vector,
 - multiplying by any of the following matrices will generate a new primitve
 - Pythagorean triple:
 -     [ 1 -2 2 ]       [ 1 2 2 ]      [ -1 2 2 ]
 - A = [ 2 -1 2 ],  B = [ 2 1 2 ], C = [ -2 1 2 ]
 -     [ 2 -2 3 ]       [ 2 2 3 ]      [ -2 2 3 ]
 -}
primitiveTriples = let trips = [3,4,5]:(concat $ map newTriples trips)
                   in trips
newTriples [a,b,c] = [ 1*a - 2*b + 2*c, 2*a - 1*b + 2*c, 2*a - 2*b + 3*c]:
                     [ 1*a + 2*b + 2*c, 2*a + 1*b + 2*c, 2*a + 2*b + 3*c]:
                     [-1*a + 2*b + 2*c,-2*a + 1*b + 2*c,-2*a + 2*b + 3*c]:
                     []

pascalTriangle = iterate nextRow [1]
    where nextRow r = zipWith (+) ([0]++r) (r ++ [0])
