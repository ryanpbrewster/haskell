module Problems.P013Test (case_013_main) where

import Test.Tasty.Discover (Assertion, (@?=))
import Problems.P013

import qualified TestData.P013

case_013_main :: Assertion
case_013_main = process TestData.P013.txt @?= "5537376230"
