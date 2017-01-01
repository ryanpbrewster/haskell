module Problems.P053Spec (spec) where

import Test.Hspec
import Problems.P053

spec :: Spec
spec = do
  describe "problem 053" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

