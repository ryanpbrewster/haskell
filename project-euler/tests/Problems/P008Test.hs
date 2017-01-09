module Problems.P008Test
  ( case_008_main
  ) where

import Problems.P008
import Test.Tasty.Discover (Assertion, (@?=))

import qualified TestData.P008

case_008_main :: Assertion
case_008_main = process TestData.P008.txt @?= "40824"
