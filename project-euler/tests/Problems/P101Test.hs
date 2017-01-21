module Problems.P101Test
  ( case_101_main
  ) where

import Problems.P101
import Test.Tasty.Discover (Assertion, (@?=))

case_101_main :: Assertion
case_101_main = solve @?= "37076114526"
