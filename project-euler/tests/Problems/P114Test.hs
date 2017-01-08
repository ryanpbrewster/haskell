module Problems.P114Test (case_114_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P114

case_114_main :: Assertion
case_114_main = solve @?= "233168"
