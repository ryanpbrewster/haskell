module Problems.P024Spec (spec) where

import Test.Hspec
import Problems.P024

spec :: Spec
spec = do
  describe "problem 024" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

