module Problems.P024Test (case_024_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P024

case_024_main :: Assertion
case_024_main = solve @?= "2783915460"
