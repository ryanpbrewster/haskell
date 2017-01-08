module Problems.P125Spec (spec) where

import Test.Hspec
import Problems.P125

spec :: Spec
spec = do
  describe "problem 125" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

