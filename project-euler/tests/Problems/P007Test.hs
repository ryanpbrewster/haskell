module Problems.P007Test (case_007_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P007

case_007_main :: Assertion
case_007_main = solve @?= "104743"
