module Problems.P122Test (case_122_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P122

case_122_main :: Assertion
case_122_main = solve @?= "233168"
