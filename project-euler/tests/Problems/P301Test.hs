module Problems.P301Test (case_301_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P301

case_301_main :: Assertion
case_301_main = solve @?= "233168"
