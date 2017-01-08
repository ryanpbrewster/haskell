module Problems.P035Spec (spec) where

import Test.Hspec
import Problems.P035

spec :: Spec
spec = do
  describe "problem 035" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

