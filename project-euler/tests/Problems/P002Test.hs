module Problems.P002Test
  ( case_002_main
  ) where

import Problems.P002
import Test.Tasty.Discover (Assertion, (@?=))

case_002_main :: Assertion
case_002_main = solve @?= "4613732"
