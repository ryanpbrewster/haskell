module Problems.P114Spec (spec) where

import Test.Hspec
import Problems.P114

spec :: Spec
spec = do
  describe "problem 114" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

