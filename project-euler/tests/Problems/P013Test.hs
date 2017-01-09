module Problems.P013Test
  ( case_013_main
  ) where

import Problems.P013
import Test.Tasty.Discover (Assertion, (@?=))

import qualified TestData.P013

case_013_main :: Assertion
case_013_main = process TestData.P013.txt @?= "5537376230"
