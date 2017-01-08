module Problems.P203Test (case_203_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P203

case_203_main :: Assertion
case_203_main = solve @?= "233168"
