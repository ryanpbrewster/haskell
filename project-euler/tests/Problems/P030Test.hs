module Problems.P030Test (case_030_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P030

case_030_main :: Assertion
case_030_main = solve @?= "233168"
