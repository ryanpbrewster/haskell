module Problems.P052Test (case_052_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P052

case_052_main :: Assertion
case_052_main = solve @?= "233168"
