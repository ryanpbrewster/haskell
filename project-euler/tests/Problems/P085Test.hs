module Problems.P085Test (case_085_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P085

case_085_main :: Assertion
case_085_main = solve @?= "233168"
