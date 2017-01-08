module Problems.P019Spec (spec) where

import Test.Hspec
import Problems.P019

spec :: Spec
spec = do
  describe "problem 019" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

