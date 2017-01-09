module Problems.P072Test
  ( case_072_main
  ) where

import Problems.P072
import Test.Tasty.Discover (Assertion, (@?=))

case_072_main :: Assertion
case_072_main = solve @?= "233168"
