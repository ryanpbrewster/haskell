module Problems.P048Test (case_048_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P048

case_048_main :: Assertion
case_048_main = solve @?= "9110846700"
