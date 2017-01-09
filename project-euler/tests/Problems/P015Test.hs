module Problems.P015Test
  ( case_015_main
  ) where

import Problems.P015
import Test.Tasty.Discover (Assertion, (@?=))

case_015_main :: Assertion
case_015_main = solve @?= "137846528820"
