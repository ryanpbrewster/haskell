module Problems.P317Test (case_317_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P317

case_317_main :: Assertion
case_317_main = solve @?= "233168"
