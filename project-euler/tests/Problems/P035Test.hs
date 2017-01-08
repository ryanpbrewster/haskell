module Problems.P035Test (case_035_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P035

case_035_main :: Assertion
case_035_main = solve @?= "233168"
