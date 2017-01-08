module Problems.P132Test (case_132_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P132

case_132_main :: Assertion
case_132_main = solve @?= "233168"
