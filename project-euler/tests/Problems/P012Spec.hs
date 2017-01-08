module Problems.P012Spec (spec) where

import Test.Hspec
import Problems.P012

spec :: Spec
spec = do
  describe "problem 012" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

