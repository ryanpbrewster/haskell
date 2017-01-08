module Problems.P056Test (case_056_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P056

case_056_main :: Assertion
case_056_main = solve @?= "233168"
