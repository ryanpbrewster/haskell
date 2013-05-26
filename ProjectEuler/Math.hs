module ProjectEuler.Math
( powerMod
, integerDigits
, binomial
--, realDigits
) where

import ProjectEuler.Prime (factors)
import Data.List (nub)

powerMod _ 0 _ = 1
powerMod x y m = let t = powerMod x (y `quot` 2) m
                     t2 = t*t
                 in case (y `mod` 2) of
                    0 -> t2 `mod` m
                    1 -> (t2*x) `mod` m

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
integerDigits = reverse.integerDigits'
integerDigits' 0 = []
integerDigits' n = let (q,r) = n `quotRem` 10
                   in r:(integerDigits q)

-- binomial n k == n!/k!(n-k)!
--              == n*(n-1)*...*(n-k+1)/(1*2*...*k)
binomial n k = let k' = min k (n-k)
               in (product [n-k'+1..n]) `div` (product [1..k'])
