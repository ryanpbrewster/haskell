module Problems.P047Spec (spec) where

import Test.Hspec
import Problems.P047

spec :: Spec
spec = do
  describe "problem 047" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

