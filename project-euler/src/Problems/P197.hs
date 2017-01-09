module Problems.P197
  ( solve
  ) where

{-
 - Given is the function f(x) = Floor[2^(30.403243784-x^2)] * 10^-9
 -
 - The sequence u[n] is defined by u[0] = -1 and u[n+1] = f(u[n]).
 -
 - Find u[n] + u[n+1] for n = 10^12.
 - Give your answer with 9 digits after the decimal point.
 -}
{-
 - A few simple notes:
 -     f(x) = Floor[ 1.42*10^9 * 2^(-x^2) ] * 10^(-9)
 -
 - Also note: u[n] converges to the very simple sequence
 - [1.029461842,0.681175875,1.029461842,0.681175875,...]
 -
 - That is, it just jumps back and forth between those two values forever.
 - Thus, u[n] + u[n+1] == 1.029461842 + 0.681175875 == 1.710637717
 -}
import Text.Printf

solve :: String
solve = printf "%.9f\n" solveProblem

f :: Double -> Double
f x = (fromIntegral $ floor (1.42e9 * (2 ** (-x ** 2)))) * 1.0e-9

solveProblem =
  let u = iterate f (-1.0)
                   -- drop 1000 so that it converges
  in sum $ take 2 $ drop 1000 $ u
