module Problems.P116Test
  ( case_116_main
  ) where

import Problems.P116
import Test.Tasty.Discover (Assertion, (@?=))

case_116_main :: Assertion
case_116_main = solve @?= "20492570929"
