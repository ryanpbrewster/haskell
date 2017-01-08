module Problems.P028Test (case_028_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P028

case_028_main :: Assertion
case_028_main = solve @?= "233168"
