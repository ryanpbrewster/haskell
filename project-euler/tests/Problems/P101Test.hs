module Problems.P101Test (case_101_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P101

case_101_main :: Assertion
case_101_main = solve @?= "233168"
