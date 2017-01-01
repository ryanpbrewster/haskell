module Problems.P110Spec (spec) where

import Test.Hspec
import Problems.P110

spec :: Spec
spec = do
  describe "problem 110" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

