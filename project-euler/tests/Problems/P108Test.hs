module Problems.P108Test (case_108_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P108

case_108_main :: Assertion
case_108_main = solve @?= "233168"
