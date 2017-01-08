module Problems.P181Test (case_181_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P181

case_181_main :: Assertion
case_181_main = solve @?= "233168"
