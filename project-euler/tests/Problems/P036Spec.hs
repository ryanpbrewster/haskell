module Problems.P036Spec (spec) where

import Test.Hspec
import Problems.P036

spec :: Spec
spec = do
  describe "problem 036" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

