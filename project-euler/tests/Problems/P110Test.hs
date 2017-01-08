module Problems.P110Test (case_110_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P110

case_110_main :: Assertion
case_110_main = solve @?= "233168"
