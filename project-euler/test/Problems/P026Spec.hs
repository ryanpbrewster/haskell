module Problems.P026Spec (spec) where

import Test.Hspec
import Problems.P026

spec :: Spec
spec = do
  describe "problem 026" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

