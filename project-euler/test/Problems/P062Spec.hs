module Problems.P062Spec (spec) where

import Test.Hspec
import Problems.P062

spec :: Spec
spec = do
  describe "problem 062" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

