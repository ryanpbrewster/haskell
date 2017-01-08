module Problems.P055Spec (spec) where

import Test.Hspec
import Problems.P055

spec :: Spec
spec = do
  describe "problem 055" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

