module Problems.P007Spec (spec) where

import Test.Hspec
import Problems.P007

spec :: Spec
spec = do
  describe "problem 007" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "104743"
