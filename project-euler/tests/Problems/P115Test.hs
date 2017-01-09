module Problems.P115Test
  ( case_115_main
  ) where

import Problems.P115
import Test.Tasty.Discover (Assertion, (@?=))

case_115_main :: Assertion
case_115_main = solve @?= "233168"
