module Problems.P079Test
  ( case_079_main
  ) where

import Problems.P079
import Test.Tasty.Discover (Assertion, (@?=))
import qualified TestData.P079

case_079_main :: Assertion
case_079_main = process TestData.P079.txt @?= "73162890"
