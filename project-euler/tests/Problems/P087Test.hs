module Problems.P087Test
  ( case_087_main
  ) where

import Problems.P087
import Test.Tasty.Discover (Assertion, (@?=))

case_087_main :: Assertion
case_087_main = solve @?= "1097343"
