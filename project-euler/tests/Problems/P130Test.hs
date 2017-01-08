module Problems.P130Test (case_130_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P130

case_130_main :: Assertion
case_130_main = solve @?= "233168"
