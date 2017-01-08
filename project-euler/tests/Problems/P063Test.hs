module Problems.P063Test (case_063_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P063

case_063_main :: Assertion
case_063_main = solve @?= "233168"
