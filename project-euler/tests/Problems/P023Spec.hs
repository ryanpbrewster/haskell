module Problems.P023Spec (spec) where

import Test.Hspec
import Problems.P023

spec :: Spec
spec = do
  describe "problem 023" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

