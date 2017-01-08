module Problems.P250Test (case_250_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P250

case_250_main :: Assertion
case_250_main = solve @?= "233168"
