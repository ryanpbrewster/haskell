module Problems.P149Spec (spec) where

import Test.Hspec
import Problems.P149

spec :: Spec
spec = do
  describe "problem 149" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

