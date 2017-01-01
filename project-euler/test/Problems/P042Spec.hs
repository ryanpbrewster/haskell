module Problems.P042Spec (spec) where

import Test.Hspec
import Problems.P042

spec :: Spec
spec = do
  describe "problem 042" $ do
    it "produces the correct answer" $ do
      process "foo" `shouldBe` "233168"
