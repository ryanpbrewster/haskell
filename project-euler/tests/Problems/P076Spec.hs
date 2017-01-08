module Problems.P076Spec (spec) where

import Test.Hspec
import Problems.P076

spec :: Spec
spec = do
  describe "problem 076" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

