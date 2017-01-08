module Problems.P057Spec (spec) where

import Test.Hspec
import Problems.P057

spec :: Spec
spec = do
  describe "problem 057" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

