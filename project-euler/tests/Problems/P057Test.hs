module Problems.P057Test
  ( case_057_main
  ) where

import Problems.P057
import Test.Tasty.Discover (Assertion, (@?=))

case_057_main :: Assertion
case_057_main = solve @?= "233168"
