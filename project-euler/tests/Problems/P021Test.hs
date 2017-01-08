module Problems.P021Test (case_021_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P021

case_021_main :: Assertion
case_021_main = solve @?= "233168"
