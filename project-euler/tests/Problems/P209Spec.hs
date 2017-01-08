module Problems.P209Spec (spec) where

import Test.Hspec
import Problems.P209

spec :: Spec
spec = do
  describe "problem 209" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

