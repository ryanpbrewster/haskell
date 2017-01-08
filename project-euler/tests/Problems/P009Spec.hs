module Problems.P009Spec (spec) where

import Test.Hspec
import Problems.P009

spec :: Spec
spec = do
  describe "problem 009" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "31875000"
