module Problems.P002Spec (spec) where

import Test.Hspec
import Problems.P002

spec :: Spec
spec = do
  describe "problem 002" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "4613732"
