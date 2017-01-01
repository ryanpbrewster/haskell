module Problems.P037Spec (spec) where

import Test.Hspec
import Problems.P037

spec :: Spec
spec = do
  describe "problem 037" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

