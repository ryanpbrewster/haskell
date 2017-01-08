module Problems.P206Test (case_206_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P206

case_206_main :: Assertion
case_206_main = solve @?= "233168"
