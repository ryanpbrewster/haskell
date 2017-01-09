module Problems.P031Test (case_031_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P031

case_031_main :: Assertion
case_031_main = solve @?= "73682"
