module Problems.P181Test
  ( case_181_main
  ) where

import Problems.P181
import Test.Tasty.Discover (Assertion, (@?=))

case_181_main :: Assertion
case_181_main = solve @?= "233168"
