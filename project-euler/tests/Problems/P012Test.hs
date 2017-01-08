module Problems.P012Test (case_012_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P012

case_012_main :: Assertion
case_012_main = solve @?= "76576500"
