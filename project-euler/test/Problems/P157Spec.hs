module Problems.P157Spec (spec) where

import Test.Hspec
import Problems.P157

spec :: Spec
spec = do
  describe "problem 157" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

