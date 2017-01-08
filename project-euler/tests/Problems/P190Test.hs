module Problems.P190Test (case_190_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P190

case_190_main :: Assertion
case_190_main = solve @?= "233168"
