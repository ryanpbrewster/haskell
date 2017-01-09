module Problems.P235Test
  ( case_235_main
  ) where

import Problems.P235
import Test.Tasty.Discover (Assertion, (@?=))

case_235_main :: Assertion
case_235_main = solve @?= "233168"
