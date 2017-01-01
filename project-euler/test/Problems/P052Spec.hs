module Problems.P052Spec (spec) where

import Test.Hspec
import Problems.P052

spec :: Spec
spec = do
  describe "problem 052" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

