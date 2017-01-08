module Problems.P044Test (case_044_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P044

case_044_main :: Assertion
case_044_main = solve @?= "233168"
