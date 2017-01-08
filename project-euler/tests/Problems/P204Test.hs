module Problems.P204Test (case_204_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P204

case_204_main :: Assertion
case_204_main = solve @?= "233168"
