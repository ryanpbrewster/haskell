module Problems.P190Spec (spec) where

import Test.Hspec
import Problems.P190

spec :: Spec
spec = do
  describe "problem 190" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

