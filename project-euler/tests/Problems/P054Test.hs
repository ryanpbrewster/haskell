module Problems.P054Test
  ( case_054_main
  ) where

import Problems.P054
import Test.Tasty.Discover (Assertion, (@?=))
import qualified TestData.P054

case_054_main :: Assertion
case_054_main = process TestData.P054.txt @?= "376"
