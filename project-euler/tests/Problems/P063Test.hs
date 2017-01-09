module Problems.P063Test
  ( case_063_main
  ) where

import Problems.P063
import Test.Tasty.Discover (Assertion, (@?=))

case_063_main :: Assertion
case_063_main = solve @?= "233168"
