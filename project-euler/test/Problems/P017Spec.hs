module Problems.P017Spec (spec) where

import Test.Hspec
import Problems.P017

spec :: Spec
spec = do
  describe "problem 017" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"
