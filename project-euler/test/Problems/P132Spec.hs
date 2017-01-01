module Problems.P132Spec (spec) where

import Test.Hspec
import Problems.P132

spec :: Spec
spec = do
  describe "problem 132" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

