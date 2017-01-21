module Util.Math
  ( binomial
  , coinCombos
  , coinCombosP
  , factorial
  , fromIntegerDigits
  , fromIntegerDigitsBy
  , fullCoinCombos
  , fromContinuedFraction
  , integerDigits
  , integerDigitsBy
  , pascalTriangle
  , powerMod
  , primitiveTriples
  , reverseDigits
  , reverseDigitsBy
  ) where

import Data.List (nub, unfoldr)

factorial n = product [2 .. n]

powerMod _ 0 _ = 1
powerMod x y m
  | even y = ((powerMod x (y `div` 2) m) ^ 2) `mod` m
  | odd y = (x * (powerMod x (y `div` 2) m) ^ 2) `mod` m

-- NB: integerDigits 0 == []
integerDigits = integerDigitsBy 10

integerDigitsBy
  :: Integral a
  => a -> a -> [a]
integerDigitsBy base = unfoldr nextDigit
  where
    nextDigit 0 = Nothing
    nextDigit n =
      let (q, r) = n `divMod` base
      in Just (r, q)

reverseDigits = reverseDigitsBy 10

reverseDigitsBy :: Integer -> Integer -> Integer
reverseDigitsBy base n = f n 0
  where
    f 0 y = y
    f x y =
      let (q, r) = x `divMod` base
      in f q (base * y + r)

fromIntegerDigits = fromIntegerDigitsBy 10

fromIntegerDigitsBy
  :: Integral a
  => a -> [a] -> a
fromIntegerDigitsBy base = foldr acc 0
  where
    acc a b = a + base * b

-- binomial n k == n!/k!(n-k)!
--              == n*(n-1)*...*(n-k+1)/(1*2*...*k)
binomial n k
  | k < 0 || k > n = 0
  | otherwise =
    let k' = min k (n - k)
    in (product [n - k' + 1 .. n]) `div` (product [1 .. k'])

{-
 - Coin combinations deals with problems like:
 -     How many ways can you make 100 using only elements from {1,5,10,25}
 -     What are the integer partitions of 10?
 -
 - coinCombos gives a list of the NUMBER OF COMBINATIONS
 - fullCoinCOmbos gives a list of the ACTUAL COMBINATIONS
 -}
coinCombos coins = coinCombos' coins (1 : repeat 0) 0

coinCombos' [] combos front = drop front combos
coinCombos' (c:cs) combos front =
  let (start, rest) = splitAt c combos
      combos' = start ++ zipWith (+) rest combos'
      start' = drop front start
  in start' ++ (coinCombos' cs combos' c)

fullCoinCombos coins = fullCoinCombos' coins ([[]] : repeat []) 0

fullCoinCombos' [] combos front = drop front combos
fullCoinCombos' (c:cs) combos front =
  let (start, rest) = splitAt c combos
      combos' = start ++ zipWith (newCombos c) rest combos'
      start' = drop front start
  in start' ++ (fullCoinCombos' cs combos' c)

newCombos c combos1 combos2 = combos1 ++ [c : c2 | c2 <- combos2]

-- proper coin combos. Can only use each coin once
coinCombosP coins = coinCombosP' coins (1 : repeat 0) 0

coinCombosP' [] combos front = drop front combos
coinCombosP' (x:xs) combos front =
  let (start, rest) = splitAt x combos
      combos' = start ++ zipWith (+) rest combos
      start' = drop front start
  in start' ++ (coinCombosP' xs combos' x)

-- Returns a (n,d) pair which represents n/d in reduced form.
fromContinuedFraction [a] = (a, 1)
fromContinuedFraction (a:cfrac) =
  let (rn, rd) = fromContinuedFraction cfrac
      n = rn * a + rd
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
primitiveTriples =
  let trips = [3, 4, 5] : (concat $ map newTriples trips)
  in trips

newTriples [a, b, c] =
  [1 * a - 2 * b + 2 * c, 2 * a - 1 * b + 2 * c, 2 * a - 2 * b + 3 * c] :
  [1 * a + 2 * b + 2 * c, 2 * a + 1 * b + 2 * c, 2 * a + 2 * b + 3 * c] :
  [-1 * a + 2 * b + 2 * c, -2 * a + 1 * b + 2 * c, -2 * a + 2 * b + 3 * c] : []

pascalTriangle = iterate nextRow [1]
  where
    nextRow r = zipWith (+) ([0] ++ r) (r ++ [0])
