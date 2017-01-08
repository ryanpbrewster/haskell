module Problems.P051Test (case_051_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P051

case_051_main :: Assertion
case_051_main = solve @?= "233168"
