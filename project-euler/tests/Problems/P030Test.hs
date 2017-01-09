module Problems.P030Test
  ( case_030_main
  ) where

import Problems.P030
import Test.Tasty.Discover (Assertion, (@?=))

case_030_main :: Assertion
case_030_main = solve @?= "443839"
