module Problems.P072Test
  ( test_072
  ) where

import Problems.P072
import Test.Tasty.Discover

test_072 :: [TestTree]
test_072 =
  [ testCase "correctness" $ fastSolve (10^4) @?= bruteForceSolve (10^4)
  , testCase "main" $ solve @?= "303963552391"
  ]
