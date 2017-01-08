module Problems.P062Test (case_062_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P062

case_062_main :: Assertion
case_062_main = solve @?= "233168"
