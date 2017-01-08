module Problems.P122Spec (spec) where

import Test.Hspec
import Problems.P122

spec :: Spec
spec = do
  describe "problem 122" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

