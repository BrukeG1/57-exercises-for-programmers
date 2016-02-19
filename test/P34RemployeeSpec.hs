module P34RemployeeSpec (main,spec) where

import Test.Hspec
import P34Remployee hiding (main)

main :: IO ()
main = hspec spec

--TODO: Implement tests for guessNum (hard because IO)
spec :: Spec
spec = do
    describe "remp" $ do
      it "should remove \"Bob\" from list [\"Bill\",\"Bob\"]" $ do
        remp "Bob" ["Bill","Bob"] `shouldBe` ["Bill"]
      it "should remove \"Bob\" from list [\"Bob\",\"Bill\"]" $ do
        remp "Bob" ["Bill","Bob"] `shouldBe` ["Bill"]

