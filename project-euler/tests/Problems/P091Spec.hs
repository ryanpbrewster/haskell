module Problems.P091Spec (spec) where

import Test.Hspec
import Problems.P091

spec :: Spec
spec = do
  describe "problem 091" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

