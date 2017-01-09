module Problems.P047Test
  ( case_047_main
  ) where

import Problems.P047
import Test.Tasty.Discover (Assertion, (@?=))

case_047_main :: Assertion
case_047_main = solve @?= "134043"
