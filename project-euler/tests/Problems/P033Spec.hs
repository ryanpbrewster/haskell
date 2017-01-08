module Problems.P033Spec (spec) where

import Test.Hspec
import Problems.P033

spec :: Spec
spec = do
  describe "problem 033" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

