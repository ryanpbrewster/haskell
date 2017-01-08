module Problems.P124Test (case_124_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P124

case_124_main :: Assertion
case_124_main = solve @?= "233168"
