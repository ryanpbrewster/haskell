module Problems.P009Test
  ( case_009_main
  ) where

import Problems.P009
import Test.Tasty.Discover (Assertion, (@?=))

case_009_main :: Assertion
case_009_main = solve @?= "31875000"
