module Problems.P027Test
  ( case_027_main
  ) where

import Problems.P027
import Test.Tasty.Discover (Assertion, (@?=))

case_027_main :: Assertion
case_027_main = solve @?= "-59231"
