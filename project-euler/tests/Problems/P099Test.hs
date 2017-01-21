module Problems.P099Test
  ( case_099_main
  ) where

import Problems.P099
import Test.Tasty.Discover (Assertion, (@?=))
import qualified TestData.P099

case_099_main :: Assertion
case_099_main = process TestData.P099.txt @?= "709"
