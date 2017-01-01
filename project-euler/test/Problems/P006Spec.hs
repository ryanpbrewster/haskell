module Problems.P006Spec (spec) where

import Test.Hspec
import Problems.P006

spec :: Spec
spec = do
  describe "problem 006" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

