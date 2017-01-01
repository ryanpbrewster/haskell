module Problems.P005Spec (spec) where

import Test.Hspec
import Problems.P005

spec :: Spec
spec = do
  describe "problem 005" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

