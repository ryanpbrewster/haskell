module Problems.P188Test
  ( case_188_main
  ) where

import Problems.P188
import Test.Tasty.Discover (Assertion, (@?=))

case_188_main :: Assertion
case_188_main = solve @?= "233168"
