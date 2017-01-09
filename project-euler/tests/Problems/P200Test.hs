module Problems.P200Test
  ( case_200_main
  ) where

import Problems.P200
import Test.Tasty.Discover (Assertion, (@?=))

case_200_main :: Assertion
case_200_main = solve @?= "233168"
