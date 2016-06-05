module Main where

import Data.Maybe
import Data.List

data JamCoin = JamCoin String [Integer] deriving (Show)

validateJamCoin :: Integer -> Maybe JamCoin
validateJamCoin n =
  let
    bitstr = bitString n
    interpretations = [ interpret b bitstr | b <- [2..10] ]
    evidence = map nonTrivialDivisor interpretations
  in
    if all isJust evidence 
      then Just (JamCoin bitstr $ catMaybes evidence) 
      else Nothing

bitString :: Integer -> String
bitString 0 = "0"
bitString n = reverse (build n)
  where
  build 0 = ""
  build n = (if odd n then '1' else '0') : build (n `div` 2)

interpret :: Integer -> String -> Integer
interpret b xs = foldl build 0 xs
  where
  build acc x = b * acc + (if x == '1' then 1 else 0)

primes :: [Integer]
primes = 2 : filter isPrime [3,5..]

isPrime :: Integer -> Bool
isPrime = isNothing . nonTrivialDivisor

nonTrivialDivisor :: Integer -> Maybe Integer
nonTrivialDivisor n = find (\p -> n `mod` p == 0) $ takeWhile (\p -> p*p <= n) primes
