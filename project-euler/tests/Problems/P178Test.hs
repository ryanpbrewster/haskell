module Problems.P178Test
  ( case_178_main
  ) where

import Problems.P178
import Test.Tasty.Discover (Assertion, (@?=))

case_178_main :: Assertion
case_178_main = solve @?= "233168"
