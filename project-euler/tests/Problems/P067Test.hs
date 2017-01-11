module Problems.P067Test
  ( case_067_main
  ) where

import Problems.P067
import qualified TestData.P067
import Test.Tasty.Discover (Assertion, (@?=))

case_067_main :: Assertion
case_067_main = process TestData.P067.txt @?= "7273"
