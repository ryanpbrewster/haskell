module Problems.P125Test (case_125_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P125

case_125_main :: Assertion
case_125_main = solve @?= "233168"
