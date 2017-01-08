module Problems.P197Test (case_197_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P197

case_197_main :: Assertion
case_197_main = solve @?= "233168"
