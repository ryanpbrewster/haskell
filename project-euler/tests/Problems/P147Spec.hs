module Problems.P147Spec (spec) where

import Test.Hspec
import Problems.P147

spec :: Spec
spec = do
  describe "problem 147" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

