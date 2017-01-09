module Problems.P183Test
  ( case_183_main
  ) where

import Problems.P183
import Test.Tasty.Discover (Assertion, (@?=))

case_183_main :: Assertion
case_183_main = solve @?= "233168"
