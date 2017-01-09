module Problems.P038Test (case_038_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P038

case_038_main :: Assertion
case_038_main = solve @?= "987654321"
