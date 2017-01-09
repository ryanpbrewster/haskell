module Problems.P039Test (case_039_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P039

case_039_main :: Assertion
case_039_main = solve @?= "840"
