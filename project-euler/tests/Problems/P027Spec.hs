module Problems.P027Spec (spec) where

import Test.Hspec
import Problems.P027

spec :: Spec
spec = do
  describe "problem 027" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

