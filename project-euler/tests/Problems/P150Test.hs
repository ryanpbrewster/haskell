module Problems.P150Test
  ( case_150_main
  ) where

import Problems.P150
import Test.Tasty.Discover (Assertion, (@?=))

case_150_main :: Assertion
case_150_main = solve @?= "233168"
