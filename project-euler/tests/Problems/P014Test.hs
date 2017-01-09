module Problems.P014Test
  ( case_014_main
  ) where

import Problems.P014
import Test.Tasty.Discover (Assertion, (@?=))

case_014_main :: Assertion
case_014_main = solve @?= "837799"
