module Problems.P077Test
  ( case_077_main
  ) where

import Problems.P077
import Test.Tasty.Discover (Assertion, (@?=))

case_077_main :: Assertion
case_077_main = solve @?= "71"
