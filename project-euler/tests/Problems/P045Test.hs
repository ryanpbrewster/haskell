module Problems.P045Test (case_045_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P045

case_045_main :: Assertion
case_045_main = solve @?= "233168"
