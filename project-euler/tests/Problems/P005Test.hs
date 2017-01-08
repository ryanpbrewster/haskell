module Problems.P005Test (case_005_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P005

case_005_main :: Assertion
case_005_main = solve @?= "233168"
