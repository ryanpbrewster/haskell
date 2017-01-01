module Problems.P317Spec (spec) where

import Test.Hspec
import Problems.P317

spec :: Spec
spec = do
  describe "problem 317" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

