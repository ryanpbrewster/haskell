module Problems.P201Test
  ( case_201_main
  ) where

import Problems.P201
import Test.Tasty.Discover (Assertion, (@?=))

case_201_main :: Assertion
case_201_main = solve @?= "233168"
