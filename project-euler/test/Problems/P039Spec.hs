module Problems.P039Spec (spec) where

import Test.Hspec
import Problems.P039

spec :: Spec
spec = do
  describe "problem 039" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

