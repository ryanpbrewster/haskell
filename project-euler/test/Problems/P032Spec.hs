module Problems.P032Spec (spec) where

import Test.Hspec
import Problems.P032

spec :: Spec
spec = do
  describe "problem 032" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

