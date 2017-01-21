module Problems.P059Test
  ( case_059_main
  ) where

import Problems.P059
import Test.Tasty.Discover (Assertion, (@?=))
import qualified TestData.P059

case_059_main :: Assertion
case_059_main = process TestData.P059.txt @?= "107359"
