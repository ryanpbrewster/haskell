module Problems.P001Test (case_001_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P001

case_001_main :: Assertion
case_001_main = solve @?= "233168"
