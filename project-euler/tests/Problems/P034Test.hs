module Problems.P034Test (case_034_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P034

case_034_main :: Assertion
case_034_main = solve @?= "40730"
