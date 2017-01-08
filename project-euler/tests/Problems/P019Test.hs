module Problems.P019Test (case_019_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P019

case_019_main :: Assertion
case_019_main = solve @?= "171"
