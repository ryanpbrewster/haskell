module Problems.P091Test (case_091_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P091

case_091_main :: Assertion
case_091_main = solve @?= "233168"
