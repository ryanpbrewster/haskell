module Problems.P157Test (case_157_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P157

case_157_main :: Assertion
case_157_main = solve @?= "233168"
