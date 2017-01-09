module Problems.P026Test
  ( case_026_main
  ) where

import Problems.P026
import Test.Tasty.Discover (Assertion, (@?=))

case_026_main :: Assertion
case_026_main = solve @?= "983"
