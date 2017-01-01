module Problems.P003Spec (spec) where

import Test.Hspec
import Problems.P003

spec :: Spec
spec = do
  describe "problem 003" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

