module Problems.P124Spec (spec) where

import Test.Hspec
import Problems.P124

spec :: Spec
spec = do
  describe "problem 124" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

