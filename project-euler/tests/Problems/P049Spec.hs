module Problems.P049Spec (spec) where

import Test.Hspec
import Problems.P049

spec :: Spec
spec = do
  describe "problem 049" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

