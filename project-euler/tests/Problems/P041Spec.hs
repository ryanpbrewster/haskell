module Problems.P041Spec (spec) where

import Test.Hspec
import Problems.P041

spec :: Spec
spec = do
  describe "problem 041" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

