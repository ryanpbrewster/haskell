module Problems.P017Test
  ( case_017_main
  ) where

import Problems.P017
import Test.Tasty.Discover (Assertion, (@?=))

case_017_main :: Assertion
case_017_main = solve @?= "21124"
