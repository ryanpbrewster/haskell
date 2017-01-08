module Problems.P164Spec (spec) where

import Test.Hspec
import Problems.P164

spec :: Spec
spec = do
  describe "problem 164" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

