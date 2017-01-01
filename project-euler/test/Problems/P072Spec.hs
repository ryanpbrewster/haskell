module Problems.P072Spec (spec) where

import Test.Hspec
import Problems.P072

spec :: Spec
spec = do
  describe "problem 072" $ do
    it "produces the correct answer" $ do
      solve `shouldBe` "233168"

