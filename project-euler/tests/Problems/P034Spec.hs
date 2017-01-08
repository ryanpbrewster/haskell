module Problems.P034Spec (spec) where

import Test.Hspec
import Problems.P034

spec :: Spec
spec = do
  describe "problem 034" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

