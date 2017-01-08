module Problems.P085Spec (spec) where

import Test.Hspec
import Problems.P085

spec :: Spec
spec = do
  describe "problem 085" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

