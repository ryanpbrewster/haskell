module Problems.P044Test
  ( case_044_main
  ) where

import Problems.P044
import Test.Tasty.Discover (Assertion, (@?=))

case_044_main :: Assertion
case_044_main = solve @?= "5482660"
