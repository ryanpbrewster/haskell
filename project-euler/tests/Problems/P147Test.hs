module Problems.P147Test
  ( case_147_main
  ) where

import Problems.P147
import Test.Tasty.Discover (Assertion, (@?=))

case_147_main :: Assertion
case_147_main = solve @?= "233168"
