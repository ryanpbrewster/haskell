module Problems.P301Spec (spec) where

import Test.Hspec
import Problems.P301

spec :: Spec
spec = do
  describe "problem 301" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

