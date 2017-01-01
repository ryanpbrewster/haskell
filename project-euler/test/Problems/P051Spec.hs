module Problems.P051Spec (spec) where

import Test.Hspec
import Problems.P051

spec :: Spec
spec = do
  describe "problem 051" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

