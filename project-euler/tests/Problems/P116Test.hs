module Problems.P116Test (case_116_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P116

case_116_main :: Assertion
case_116_main = solve @?= "233168"
