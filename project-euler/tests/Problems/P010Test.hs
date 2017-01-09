module Problems.P010Test
  ( case_010_main
  ) where

import Problems.P010
import Test.Tasty.Discover (Assertion, (@?=))

case_010_main :: Assertion
case_010_main = solve @?= "142913828922"
