module Problems.P030Spec (spec) where

import Test.Hspec
import Problems.P030

spec :: Spec
spec = do
  describe "problem 030" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

