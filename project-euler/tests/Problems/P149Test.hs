module Problems.P149Test (case_149_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P149

case_149_main :: Assertion
case_149_main = solve @?= "233168"
