module Problems.P001Spec (spec) where

import Test.Hspec
import Problems.P001

spec :: Spec
spec = do
  describe "problem 001" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

