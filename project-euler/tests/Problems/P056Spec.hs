module Problems.P056Spec (spec) where

import Test.Hspec
import Problems.P056

spec :: Spec
spec = do
  describe "problem 056" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

