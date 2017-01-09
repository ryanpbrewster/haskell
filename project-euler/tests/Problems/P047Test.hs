module Problems.P047Test (case_047_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P047

case_047_main :: Assertion
case_047_main = solve @?= "134043"
