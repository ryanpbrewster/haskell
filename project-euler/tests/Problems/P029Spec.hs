module Problems.P029Spec (spec) where

import Test.Hspec
import Problems.P029

spec :: Spec
spec = do
  describe "problem 029" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

