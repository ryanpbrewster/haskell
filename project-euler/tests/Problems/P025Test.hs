module Problems.P025Test
  ( case_025_main
  ) where

import Problems.P025
import Test.Tasty.Discover (Assertion, (@?=))

case_025_main :: Assertion
case_025_main = solve @?= "4782"
