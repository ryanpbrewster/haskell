module Problems.P250Spec (spec) where

import Test.Hspec
import Problems.P250

spec :: Spec
spec = do
  describe "problem 250" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

