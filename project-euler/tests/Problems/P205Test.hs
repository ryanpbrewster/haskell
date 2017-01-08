module Problems.P205Test (case_205_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P205

case_205_main :: Assertion
case_205_main = solve @?= "233168"
