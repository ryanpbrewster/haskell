module Problems.P205Spec (spec) where

import Test.Hspec
import Problems.P205

spec :: Spec
spec = do
  describe "problem 205" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

