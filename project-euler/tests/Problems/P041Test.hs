module Problems.P041Test
  ( case_041_main
  ) where

import Problems.P041
import Test.Tasty.Discover (Assertion, (@?=))

case_041_main :: Assertion
case_041_main = solve @?= "7652413"
