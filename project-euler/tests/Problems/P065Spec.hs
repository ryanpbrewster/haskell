module Problems.P065Spec (spec) where

import Test.Hspec
import Problems.P065

spec :: Spec
spec = do
  describe "problem 065" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

