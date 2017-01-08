module Problems.P249Spec (spec) where

import Test.Hspec
import Problems.P249

spec :: Spec
spec = do
  describe "problem 249" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

